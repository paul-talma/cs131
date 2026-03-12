import argparse
import asyncio
import json
import logging
import os
import re
import time

import aiohttp

SERVER_NAME = ''
API_URL = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?'
API_KEY = 'AIzaSyCEp51AwMi4zLnCSRZyzu3fwpExlnQNqgs'

PORT_DICT = {
    'Bailey': 10000,
    'Bona': 10001,
    'Campbell': 10002,
    'Clark': 10003,
    'Jaquez': 10004,
}

NEIGHBORS = {
    'Bailey': ['Bona', 'Campbell'],
    'Bona': ['Bailey', 'Campbell', 'Clark'],
    'Campbell': ['Bailey', 'Bona', 'Jaquez'],
    'Clark': ['Bona', 'Jaquez'],
    'Jaquez': ['Campbell', 'Clark'],
}

# maps servers to writer objects of neighbors
CONNECTIONS = {}

# maps client ids to dict of location (lat, long dict) and timestamp of last update
LOCATIONS = {}

IP_ADDRESS = '127.0.0.1'

LOCATION_REGEX = re.compile(r'[+-]\d+.\d+')

SESSION = None


async def handle_connection(reader, writer):
    while True:
        data = await reader.readline()
        if not data:
            break
        message = data.decode().strip()
        if not message:
            break

        parts = message.split()
        match parts[0]:
            case 'AT':
                await handle_at(writer, parts)
            case 'IAMAT':
                await handle_iamat(writer, parts)
            case 'WHATSAT':
                await handle_whatsat(writer, parts)
            case _:
                await handle_invalid_command(writer, parts)

    writer.close()
    logging.info(f'Closed connection to {writer.get_extra_info("peername")}')
    await writer.wait_closed()


async def handle_at(writer, parts):
    if not await validate_parts(parts, message_type='at', writer=writer):
        return
    logging.info(f'Received AT from {writer.get_extra_info("peername")}')
    client_id = parts[3]
    timestamp = float(parts[-1])
    message = ' '.join(parts) + '\n'
    global LOCATIONS
    cached_location = LOCATIONS.get(client_id, None)

    if cached_location is None or timestamp_stale(cached_location, timestamp):
        logging.info('Updating cache')
        update_cache(message)  # update if stale
        await flood(message)  # propagate only if stale


async def handle_iamat(writer, parts):
    if not await validate_parts(parts, message_type='iamat', writer=writer):
        return
    logging.info(f'Received IAMAT from {writer.get_extra_info("peername")}')
    response = make_AT_message(parts)  # response includes newline

    writer.write(response.encode())
    await writer.drain()  # respond to client

    global LOCATIONS
    client_id = parts[1]
    timestamp = float(parts[-1])
    cached_location = LOCATIONS.get(client_id, None)
    if cached_location is None or cached_location['timestamp'] < timestamp:
        update_cache(response)
        await flood(response)


async def handle_whatsat(writer, parts):
    if not await validate_parts(parts, message_type='whatsat', writer=writer):
        return

    # validate client id
    client_id = parts[1]
    global LOCATIONS
    if client_id not in LOCATIONS:
        await handle_invalid_command(writer, parts)
        return

    # AT response to client
    client_location = LOCATIONS[client_id]
    response = client_location['message']

    writer.write(response.encode())
    await writer.drain()

    # API response
    lat = client_location['latitude']
    long = client_location['longitude']
    radius = parts[2]
    bound = parts[3]
    api_response = await api_call(lat, long, radius, bound)
    writer.write((api_response + '\n\n').encode())
    await writer.drain()
    logging.info('Received API response:')
    logging.info(api_response)


async def handle_invalid_command(writer, parts):
    response = '? ' + ' '.join(parts) + '\n'
    writer.write(response.encode())
    await writer.drain()


async def validate_parts(parts, message_type, writer):
    try:
        match message_type:
            case 'at':
                assert parts[0] == 'AT'
                assert len(parts) == 6
                float(parts[-1])  # timestamp
            case 'iamat':
                assert parts[0] == 'IAMAT'
                assert len(parts) == 4
                float(parts[-1])  # timestamp
            case 'whatsat':
                assert parts[0] == 'WHATSAT'
                assert len(parts) == 4
                float(parts[2])  # radius
                int(parts[3])  # bound
        return True
    except (AssertionError, IndexError, ValueError):
        await handle_invalid_command(writer, parts)
        return False


def make_AT_message(parts):
    time_diff = time.time() - float(parts[3])
    time_str = f'{time_diff:+f}'
    parts = ['AT', SERVER_NAME, time_str] + parts[1:]
    response = ' '.join(parts) + '\n'
    return response


def update_cache(message):
    parts = message.split()
    client_id = parts[3]
    lat, long = get_location_from_AT(parts)
    lat = lat.strip('+')
    long = long.strip('+')
    timestamp = float(parts[-1])
    global LOCATIONS
    LOCATIONS[client_id] = {
        'message': message,
        'latitude': lat,  # str
        'longitude': long,  # str
        'timestamp': timestamp,
    }


def get_location_from_AT(parts):
    lat, long = LOCATION_REGEX.findall(parts[4])
    return lat, long


def timestamp_stale(cached_location, timestamp):
    return cached_location['timestamp'] < timestamp


async def send_to_neighbor(neighbor_name, message):
    writer = CONNECTIONS.get(neighbor_name)
    if writer is None:
        await connect_to_neighbor(neighbor_name)
        writer = CONNECTIONS.get(neighbor_name)
    if writer is not None:
        try:
            writer.write(message.encode())
            await writer.drain()
        except Exception as e:
            logging.error(f'Failed to send to {neighbor_name}: {e}')
            CONNECTIONS[neighbor_name] = None


async def flood(message):
    tasks = [send_to_neighbor(name, message) for name in NEIGHBORS[SERVER_NAME]]
    await asyncio.gather(*tasks, return_exceptions=True)


async def api_call(lat, long, radius, bound):
    params = {
        'location': lat + ',' + long,
        'radius': int(radius) * 1000,
        'key': API_KEY,
    }
    async with SESSION.get(API_URL, params=params) as resp:
        resp = await resp.json()
        resp = json.dumps(resp['results'][: int(bound)], indent=4)

    return resp


async def connect_to_neighbors():
    global SERVER_NAME
    global NEIGHBORS
    global CONNECTIONS
    for neighbor_name in NEIGHBORS[SERVER_NAME]:
        if neighbor_name not in CONNECTIONS:
            CONNECTIONS[neighbor_name] = None
    await asyncio.gather(*(connect_to_neighbor(n) for n in NEIGHBORS[SERVER_NAME]))


async def connect_to_neighbor(neighbor_name):
    port = PORT_DICT[neighbor_name]
    logging.info(f'Connecting to {neighbor_name}...')
    global CONNECTIONS
    try:
        reader, writer = await asyncio.open_connection(
            IP_ADDRESS,
            port,
        )
        CONNECTIONS[neighbor_name] = writer
        logging.info(f'Success: connected to {neighbor_name}')

    except (ConnectionRefusedError, OSError):
        CONNECTIONS[neighbor_name] = None
        logging.info(f'Could not connect to {neighbor_name}')


async def main():
    # argument parsing
    parser = argparse.ArgumentParser(
        prog='server',
        description='Spins up a server.',
    )
    parser.add_argument('servername')
    args = parser.parse_args()
    global SERVER_NAME
    SERVER_NAME = args.servername

    # logger
    os.makedirs('logs', exist_ok=True)
    open(f'logs/{SERVER_NAME}.log', 'w').close()
    logging.basicConfig(
        filename=f'logs/{SERVER_NAME}.log',
        encoding='utf-8',
        format='%(asctime)s %(levelname)s %(message)s',
        datefmt='%m-%d %H:%M:%S',
        level=logging.INFO,
    )

    # start aiohttp session
    global SESSION
    async with aiohttp.ClientSession() as session:
        SESSION = session

        port = PORT_DICT[SERVER_NAME]
        server = await asyncio.start_server(handle_connection, IP_ADDRESS, port)

        # connect to neighbors in background
        asyncio.create_task(connect_to_neighbors())

        async with server:
            await server.serve_forever()


if __name__ == '__main__':
    asyncio.run(main())
