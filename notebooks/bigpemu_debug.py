import struct
def recv_msg(sock):
    # Bigpemu has a 16 bytes header, with the data length
    # at positions 4:6
    header = recvall(sock, 16)
    if not header:
        return None
    raw_msglen = header[4:6]
    if not raw_msglen:
        return None
    msglen = struct.unpack('<H', raw_msglen)[0]
    # Read the message data
    return (header, msglen, recvall(sock, msglen))

def recvall(sock, n):
    # Helper function to recv n bytes or return None if EOF is hit
    data = bytearray()
    while len(data) < n:
        packet = sock.recv(n - len(data))
        if not packet:
            return None
        data.extend(packet)
    return data

def get_response(sock, num_responses):
    # There's a predefined number of responses for each command,
    # so we have to specify how many to look for.
    resp = []
    for i in range(0,num_responses):
        data = recv_msg(sock)
        if not data:
            break
        resp += [data]
    return resp

