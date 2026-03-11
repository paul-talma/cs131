import asyncio


async def handle_client(reader, writer):
    addr = writer.get_extra_info('peername')
    print(f'New connection from {addr}')

    while True:
        data = await reader.readline()
        if not data:  # client disconnected
            break

        message = data.decode().strip()
        print(f'Received: {message}')

        response = f'You said: {message}\n'
        writer.write(response.encode())
        await writer.drain()  # flush the buffer

    writer.close()
    await writer.wait_closed()
    print(f'Connection from {addr} closed')


async def main():
    server = await asyncio.start_server(handle_client, '127.0.0.1', 8888)
    async with server:
        await server.serve_forever()


asyncio.run(main())

