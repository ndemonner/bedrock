bedrock
=====================

This was my (working!) attempt to create a real-time backend for games. Highlights include
a blazingly fast pub/sub architecture and MsgPack-based RPC system I custom wrote for
Erlang. Bedrock also has a decent metric aggregation suite for performing statistical
analysis across an arbitrary set of metrics/events submitted by game clients.

It was tested successfully with the hit game Blast Monkeys--over 200,000 concurrent connections were handled at its peak..
