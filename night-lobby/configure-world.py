#!/usr/bin/env python3
"""Run once as owner after a Night Lobby World is created: python3 configure-world.py WORLD_ID.

Playbit 0.1.3 seeds volume permissions in playbit-volumes.json. Entry overrides
are applied separately through the same owner operations as the native editor.
"""

import argparse
import json
from pathlib import Path
import subprocess
import sys


def public_access(acl, permissions):
    public = {"kind": "public"}
    replacement = {"subject": public, "permissions": permissions}
    grants = list(acl["grants"])
    for index, grant in enumerate(grants):
        if grant["subject"] == public:
            grants[index] = replacement
            break
    else:
        grants.append(replacement)
    return {"grants": grants}


def configure(call, volume):
    manifest = Path(__file__).with_name("playbit-volumes.json")
    databases = json.loads(manifest.read_text())["root"]["databases"]
    entry = next(db for db in databases if db["name"] == "world-state")["id"]

    def read_entry():
        return call("entry-details", volume=volume, entry=entry)["details"]

    details = read_entry()
    record = details["entry"]
    if (
        record["deleted"]
        or record["name_conflict"]
        or record["info"]["name"] != "world-state"
        or record["info"]["kind"]["type"] != "kv_db"
    ):
        raise ValueError("This World has no active world-state KV at the configured ID.")

    previous = details["permissions"]
    acl = previous["acl"] if previous["kind"] == "replace" else details["default_permissions"]
    override = {"kind": "replace", "acl": public_access(acl, ["read", "write"])}
    if override != previous:
        call("entry-permissions", volume=volume, entry=entry, permissions=override)
    if read_entry()["permissions"] != override:
        raise ValueError("Could not verify the world-state override.")

    current = call("volume-details", volume=volume)["details"]["permissions"]
    readonly = public_access(current, ["read"])
    if readonly != current:
        call("permissions", volume=volume, acl=readonly)
    final = read_entry()
    if final["default_permissions"] != readonly or final["permissions"] != override:
        raise ValueError("Final permission readback did not match the requested grants.")
    print(f"{volume}: verified local World Public Read, world-state Public Read + Write")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("world_id")
    args = parser.parse_args()
    server = "/Applications/Playbit.app/Contents/MacOS/playbit-mcp"
    with subprocess.Popen(
        [server], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True
    ) as process:
        request_id = 0

        def request(method, params):
            nonlocal request_id
            request_id += 1
            message = {"jsonrpc": "2.0", "id": request_id, "method": method, "params": params}
            print(json.dumps(message), file=process.stdin, flush=True)
            for line in process.stdout:
                reply = json.loads(line)
                if reply.get("id") == request_id:
                    if "error" in reply:
                        raise ValueError(reply["error"]["message"])
                    return reply["result"]
            raise ValueError("Playbit closed the connection before acknowledging the request.")

        def call(op, **arguments):
            result = request("tools/call", {
                "name": "playbit",
                "arguments": {"op": op, **arguments},
            })
            message = result["content"][0]["text"]
            if result.get("isError"):
                raise ValueError(f"{op}: {message}")
            return json.loads(message)

        request("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "night-lobby-world-permissions", "version": "1"},
        })
        configure(call, args.world_id)


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError) as error:
        sys.exit(str(error))
