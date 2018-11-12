# Replicated

> :warning: This software is experimental and immature.

`Replicated` is an Elm library providing data types that:
* can be replicated across machines,
* provide strong eventual consistency.

This means, in layman terms, that program using them:
* can work on data duplicated on multiple machines,
* can continue to work while offline,
* will end up with the same state once synced.

These multiple machines _could_ be a client and a server.
However the library does not have a compatible server-side counterpart.
Therefore it makes more sense to use it with client-to-client communication like WebRTC.