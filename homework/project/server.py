import argparse
import asyncio
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
    await validate_parts(parts, message_type='at', writer=writer)
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
    await validate_parts(parts, message_type='iamat', writer=writer)
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
    # AT response to client
    client_id = parts[1]
    global LOCATIONS
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
    writer.write(str(api_response).encode())
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
            case 'iamat':
                assert parts[0] == 'IAMAT'
                assert len(parts) == 4

    except AssertionError:
        await handle_invalid_command(writer, parts)


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


async def flood(message):
    global CONNECTIONS
    for neighbor_name, writer in CONNECTIONS.items():
        if writer is not None:
            writer.write(message.encode())
            await writer.drain()
        else:
            await connect_to_neighbor(neighbor_name)
            writer = CONNECTIONS.get(neighbor_name)
            if writer is not None:
                writer.write(message.encode())
                await writer.drain()


async def api_call(lat, long, radius, bound):
    params = {'location': lat + ',' + long, 'radius': radius * 1000, 'key': API_KEY}
    # url = API_URL + f'location={lat},{long}&radius={radius}&key={API_KEY}'
    async with SESSION.get(API_URL, params=params) as resp:
        resp = await resp.text()

    return resp


async def connect_to_neighbors():
    global SERVER_NAME
    global NEIGHBORS
    for neighbor_name in NEIGHBORS[SERVER_NAME]:
        await connect_to_neighbor(neighbor_name)


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

    # start server first so neighbors can connect to us
    port = PORT_DICT[SERVER_NAME]
    server = await asyncio.start_server(handle_connection, IP_ADDRESS, port)

    # start aiohttp session
    global SESSION
    SESSION = aiohttp.ClientSession()

    # connect to neighbors in background
    asyncio.create_task(connect_to_neighbors())

    async with server:
        await server.serve_forever()

    await SESSION.close()


if __name__ == '__main__':
    asyncio.run(main())
