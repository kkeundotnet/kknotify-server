kknotify-server
======

*Run script and watch youtube!*

Simple (**naive**, **insecure**, and **not-scalable**) notification
server for Linux.  (actually a simple broadcasting server)

Build
------

1.  Install opam and dune(jbuilder).

2.  Make:

    ```
    $ git clone https://github.com/kkeundotnet/kknotify-server
    $ cd kknotify-server
    $ vi config.ml     # set host, port, and key_path
    $ make
    ```

    What is `key_path`?  As of now, only server-side broadcaster can
    send notification orders.  On the other hand, notification clients
    can only receive the orders.  For authorizing the broadcaster, the
    server stores a secret key and shares it with the server-side
    broadcaster.

3.  Run the server:

    ```
    $ _build/install/default/bin/kknotify-key-gen
    $ _build/install/default/bin/kknotify-server
    ```

4.  Send notification:

    ```
    $ _build/install/default/bin/kknotify "hi"
    ```

Systemctl service setting
------

1.  Copy scripts:

    ```
    $ cp -r script.example script
    ```

2.  Change `{{user-name}}` and `{{path-to-kknotify-server}}` in
    `kknotify-server-start` and `kknotify-server.service` to your
    owns.

3.  Read and follow commands in `script.example/README.md` for the
    service registration.

For more information:

* [https://dzone.com/articles/run-your-java-application-as-a-service-on-ubuntu](https://dzone.com/articles/run-your-java-application-as-a-service-on-ubuntu)
* [https://www.raspberrypi.org/forums/viewtopic.php?t=203616](https://www.raspberrypi.org/forums/viewtopic.php?t=203616)

******

License note: This program is distributed as the public domain.
