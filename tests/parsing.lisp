;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                  ;;;
;;; Free Software published under an MIT-like license. See LICENSE   ;;;
;;;                                                                  ;;;
;;; Copyright (c) 2012 Google, Inc.  All rights reserved.            ;;;
;;;                                                                  ;;;
;;; Original author: Alejandro Sedeño                                ;;;
;;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :qmynd-test)

#|

Dependencies to add:
• flexi-streams ✓
• babel         ✓

Tests to write
• Parsing
  • Integers
    • Decoding fixed-length integers   ✓
    • Decoding length-encoded integers ✓
    • Negative tests for decoding
      • #xfb as NULL
      • #xfe as EOF packet
      • #xff as Error packet
  • Strings
    • Decoding fixed-length strings    ✓
    • Decoding null-terminated strings ✓
    • Decoding variable-length strings ✗
    • Decoding length-encoded strings  ✓
    • Decoding rest-of-packet strings  ✗
  • Packets
    • Single packets
    • Multiple packets
    • Multi-packet single payload
    • Sequence IDs
    • Response Packets
      • OK_Packet  [00]
        • With CLIENT_PROTOCOL_41 (includes warning count)
        • Without CLIENT_PROTOCOL_41 (no warning count)
      • ERR_Packet [ff]
        • With CLIENT_PROTOCOL_41 (includes #sql-state)
        • Without CLIENT_PROTOCOL_41 (no #sql-state)
      • EOF_Packet [fe]
        • With CLIENT_PROTOCOL_41 (includes warning count and status flags)
        • Without CLIENT_PROTOCOL_41 (no warning count and status flags)
  • Status flags?
  • Character Sets
    • ASCII
    • UTF-8
    • Others? (Depend on Babel)

• Generating
  • Integers
    • Encoding fixed-length integers   ✓
    • Encoding length-encoded integers ✓
  • Strings
    • Encoding fixed-length strings
    • Encoding null-terminated strings
    • Encoding variable-length strings
    • Encoding length-encoded strings
    • Encoding rest-of-packet strings

• Connecting
  • Initial Packet / Capability Negotiation
    • Protocol::HandshakeV10 (valid since MySQL 3.21.0)
    • Be ready to support newer handshakes.
    • Error out on older Handshakes and servers that don't support CLIENT_PROTOCOL_41
  • SSL Negotiation (?)
    • Protocol::SSLRequest
  • Client Response
    • Protocol::HandshakeResponse41
  • Authentication
    • Success
    • Failure
    • Switch Method (MySQL 5.5.7+)
      • Protocol::AuthSwitchRequest
      • Protocol::OldAuthSwitchRequest (?)
      • Protocol::AuthSwitchResponse
      • Protocol::AuthMoreData …
    • Methods
      • Old Password Authentication (Insecure — Force fail by default.)
      • Secure Password Authentication

• Sending Commands
  • Text Protocol
    • [00] Sleep
    • [01] Quit
    • [02] Initialize Database
    • [03] Query
    • [04] Field List
    • [05] Create Database
    • [06] Drop Database
    • [07] Refresh
    • [08] Shutdown
    • [09] Statistics
    • [0a] Process Information
    • [0b] Connect
    • [0c] Process Kill
    • [0d] Debug
    • [0e] Ping
    • [0f] Time
    • [10] Delayed Insert
    • [11] Change User
    • [1d] Daemon
  • Prepared Statements
    • [16] Statement Prepare
    • [17] Statement Execute
    • [18] Statement Send Long Data
    • [19] Statement Close
    • [1a] Statement Reset
  • Replication Protocol (server-to-server master/slave commands; no initial support here)
    • [12] Binary Log Dump
    • [13] Table Dump
    • [14] Connect Out
    • [15] Register Slave
    • [1e] Binary Log Dump GTID
  • Stored Procedures
    • [1b] Set Option
    • [1c] Statement Fetch
|#
