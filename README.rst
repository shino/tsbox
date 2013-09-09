tsbox
=====

Library of history management and selection logic which forms the foundation of
timestamp-based transaction control.

Scope
-----

- Keeping timestamped history data, some versions may be conflicted and/or concurrent.
- Timestamp can be total ordered or partial ordered.
  The outcome of partial ordered timestamp may lead to some annoying behavior.
  Investigatin such outcome is included in this prototype's scope.
- Supplying timestamp and preserving commit log are **out of scope**.
  Nonetheless very simple (and stuid) timestamp supplier and commit log server
  is inlcuded in this repository for reference/testing.

Supposed System Description and responsibility of this repository
-----------------------------------------------------------------

Consider three components which communicate with each other and
maintain transacional data in coordinated manner.

1. Timestamp management
2. Timestamped data management
3. Commit log management

Timestamp management
^^^^^^^^^^^^^^^^^^^^

Interface:

- Reply timestamp for a request.

Timestamped data management
^^^^^^^^^^^^^^^^^^^^^^^^^^^

This library forms a part of this component.

Interface:

- Reply "current" data for a read request with a key and a timestamp.
- Preserve "new" data for a write request and reply a result, ok/busy/conflicted.

  - ok: data is preserved, no confliction
  - busy: there is already conflicted data, not preserved
  - conflicted: data is preserved but conflicted

- Garbage collection (TODO)

Commit log management
^^^^^^^^^^^^^^^^^^^^^

Interface:

- Accept "begin transaction" request and keep it with expiring time.
- Accept "commit transaction" request and modify the state.
- Accept "rollback transaction" request and modify the state.
- Accept "force abort transaction" request and judge its validity and modify the state.
  Only this kind of request can be issued from any actor, i.e. can be issued by
  a actor which is other than transaction owner.

Assumption:

- Time gap from "the absolute clock" is inside +/- `MAX_TIME_GAP`.
  We consider the case where this gap is < 5 seconds.

  TODO: VALIDITY? Can there be a actor with awfully corrupted clock?

Terminology
-----------


Transaction state transition
----------------------------

::

   O  --(begin)--> initialized
                      |
                      +--(commit)--> committed [final state]
                      |
                      +--(rollback)--> aborted [final state]
                      |
                      +--(timeout?)-->
                      |
                      +--(abort)--> aborted [final state]


License
-------

Apache License Version 2.0

Reference
---------

- Timestamp-based concurrency control

  - http://en.wikipedia.org/wiki/Timestamp-based_concurrency_control

- Multiversion concurrency control

  - http://en.wikipedia.org/wiki/Multiversion_concurrency_control

- Reed, David P. (September 21, 1978).
  "Naming and Synchronization in a Decentralized Computer System". MIT dissertation.

  - http://publications.csail.mit.edu/lcs/specpub.php?id=773
  - http://publications.csail.mit.edu/lcs/pubs/pdf/MIT-LCS-TR-205.pdf

- Lamport timestamps

  - http://en.wikipedia.org/wiki/Lamport_timestamps
  - http://research.microsoft.com/users/lamport/pubs/time-clocks.pdf

- Vector clock

  - http://en.wikipedia.org/wiki/Vector_clocks
