
# QITAB MySQL Native Driver

A MySQL client that uses the native network protocol to communicate with a MySQL
server.

## Installation

Like many libraries in Common Lisp, `QMyND` is available
in [Quicklisp](https://www.quicklisp.org/beta/). To load the driver
type `(ql:quickload 'qmynd)` in the REPL.

## Basic usage

`QMyND` is rather undocumented. The best resources for learning how it
works are its source code and the included tests.

There are two ways to connect to a MySQL database: the network socket and
the local socket. The second type of connection is possible only in case
of implementations supporting `AF_LOCAL` sockets (at the time of
writing: CCL, ECL and SBCL) with the function `mysql-local-connect`. In
our example we will use the first type, but the usage is very much similar
in both cases.

All communication with the database is done through an opaque
structure representing connection. The following invocation is
self-explanatory:

```
> (defparameter *connection*
    (qmynd:mysql-connect
     :host "localhost" :username "jack" :password "daniel" :database "mydb"))
*CONNECTION*
```

Driver supports both queries and prepared statements. To issue a simple
query we use the `mysql-query` function:

```
> (qmynd:mysql-query
   *connection*
   "CREATE TABLE super_table (id INT, name VARCHAR(255), timestamp DATETIME)")
#S(QMYND:RESPONSE-OK-PACKET
   :AFFECTED-ROWS 0
   :LAST-INSERT-ID 0
   :STATUS-FLAGS 2
   :WARNINGS 0
   :INFO "")

> (qmynd:response-ok-packet-status-flags *)
2
```

If the query doesn't return rows, the function returns the
`response-ok-packet` structure that may be read with standard
accessors, which were automatically created for the structure as
depicted in the snippet above.

For efficiency and security the user may use a prepared statement. First we
need to prepare it.

```
> (defparameter *st-insert*
    (qmynd:mysql-statement-prepare
     *connection*
     "INSERT INTO super_table VALUES (?, ?, NOW())"))
*ST-INSERT*
```

Parameters to the statement are passed with the key argument `parameters`,
which needs to be a sequence (be it a vector or a list):

```
> (qmynd:mysql-statement-execute *st-insert* :parameters #(1 "foo"))
#S(QMYND:RESPONSE-OK-PACKET
   :AFFECTED-ROWS 1
   :LAST-INSERT-ID 0
   :STATUS-FLAGS 2
   :WARNINGS 0
   :INFO "")

> (dotimes (v 10)
    (qmynd:mysql-statement-execute *st-insert* :parameters (vector v "foo"))
    (sleep 1))
NIL
```

When we are done with the statement we should close it to release
the server-side resources:

```
(qmynd:mysql-statement-close *st-insert*)
; no value
```

When we query database for rows, we receive two values: the resulting rows
sequence (try the key parameter `result-type` to customize its type) and a
vector of column descriptors. The second value is not very interesting to
us because it is part of the internal implementation.

```
> (let ((results (qmynd:mysql-query
                  *connection*
                  "SELECT * FROM super_table LIMIT 3"
                  :result-type 'list)))
    results)
((1 "foo" #<MYSQL-DATE-TIME 2017-08-30 14:47:07.000000>)
 (0 "foo" #<MYSQL-DATE-TIME 2017-08-30 14:49:17.000000>)
 (1 "foo" #<MYSQL-DATE-TIME 2017-08-30 14:49:18.000000>))
```

To process rows one-by-one with a function and discard them without
consing the result, the key parameter `row-fn` should be used. To coerce
all columns to text, set `as-text` to `T`:

```
> (qmynd:mysql-query
   *connection*
   "SELECT * FROM super_table LIMIT 3"
   :as-text t
   :row-fn (lambda (row) (print row)) 
   :result-type 'list)

("1" "foo" "2017-08-30 14:47:07") 
("0" "foo" "2017-08-30 14:49:17") 
("1" "foo" "2017-08-30 14:49:18") 
NIL
```

Note that only the returned value above is NIL; the preceding lists are
printed statements appearing in the console.

MySQL `date` and `interval` types are represented as CLOS objects on
which some methods are defined.

```
> (qmynd:universal-time-to-mysql-date-time (get-universal-time))
#<MYSQL-DATE-TIME 2017-08-30 13:24:26.000000>

> (qmynd:mysql-date-time-year *)
2017

> (qmynd:seconds-to-mysql-time-interval 100)
#<MYSQL-TIME-INTERVAL 0:01:40.000000>

> (cons (qmynd:mysql-time-interval-seconds *)
        (qmynd:mysql-time-interval-minutes *))
(40 . 1)
```

When we are done with working on our database, we may close the
connection:

```
> (qmynd:mysql-disconnect *connection*)
NIL
```

This short tutorial is meant to get your feet wet with using
MySQL. For more sophisticated usage you should read the source code
and docstrings.

## Contributing

This is a free software published under an MIT-like license (see
`LICENSE`). The project is hosted on GitHub and is developed by
individuals scattered around the world like many other FOSS projects.

To contribute you have to fork the repository located
at [https://github.com/qitab/qmynd](https://github.com/qitab/qmynd)
and issue a pull request with your commits. Ideas for improvements
may be found in a `TODO` file and issue entries on tracker hosted
with the project. Writing documentation and tests is also a useful
task which benefits the project's usefulness and stability.

<!-- ## Architecture -->
