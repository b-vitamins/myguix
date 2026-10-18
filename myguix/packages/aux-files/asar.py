# SPDX-License-Identifier: GPL-3.0-or-later
"""Extract and repack an Electron ASAR archive in place.

  asar.py extract ARCHIVE DIRECTORY
  asar.py pack ARCHIVE DIRECTORY

'pack' keeps the header of ARCHIVE (unpacked entries, links, executable
bits) and takes the contents of every packed file from DIRECTORY,
recomputing offsets, sizes and integrity hashes.
"""

import hashlib
import json
import os
import struct
import sys

BLOCK_SIZE = 4 * 1024 * 1024


def read_archive(archive):
    with open(archive, "rb") as f:
        data = f.read()
    _, header_size = struct.unpack_from("<II", data, 0)
    _, json_size = struct.unpack_from("<II", data, 8)
    header = json.loads(data[16:16 + json_size])
    return header, data, 8 + header_size


def packed_files(node, prefix=""):
    for name, entry in node.get("files", {}).items():
        path = os.path.join(prefix, name)
        if "files" in entry:
            yield from packed_files(entry, path)
        elif "offset" in entry and not entry.get("unpacked"):
            yield path, entry


def integrity(content):
    blocks = [hashlib.sha256(content[i:i + BLOCK_SIZE]).hexdigest()
              for i in range(0, max(len(content), 1), BLOCK_SIZE)]
    return {"algorithm": "SHA256",
            "hash": hashlib.sha256(content).hexdigest(),
            "blockSize": BLOCK_SIZE,
            "blocks": blocks}


def extract(archive, directory):
    header, data, base = read_archive(archive)
    for path, entry in packed_files(header):
        target = os.path.join(directory, path)
        os.makedirs(os.path.dirname(target), exist_ok=True)
        offset = base + int(entry["offset"])
        with open(target, "wb") as f:
            f.write(data[offset:offset + entry["size"]])


def pack(archive, directory):
    header, _, _ = read_archive(archive)
    contents, offset = [], 0
    for path, entry in packed_files(header):
        with open(os.path.join(directory, path), "rb") as f:
            content = f.read()
        entry["offset"] = str(offset)
        entry["size"] = len(content)
        if "integrity" in entry:
            entry["integrity"] = integrity(content)
        contents.append(content)
        offset += len(content)
    encoded = json.dumps(header, separators=(",", ":")).encode()
    payload = struct.pack("<I", len(encoded)) + encoded
    payload += b"\0" * (-len(payload) % 4)
    header_pickle = struct.pack("<I", len(payload)) + payload
    with open(archive + ".tmp", "wb") as f:
        f.write(struct.pack("<II", 4, len(header_pickle)))
        f.write(header_pickle)
        for content in contents:
            f.write(content)
    os.replace(archive + ".tmp", archive)


if __name__ == "__main__":
    command, archive, directory = sys.argv[1:4]
    {"extract": extract, "pack": pack}[command](archive, directory)
