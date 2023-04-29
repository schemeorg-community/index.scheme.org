(((name . "make-client-socket")
  (signature case-lambda
             (((string? node) (string? service)) socket?)
             (((string? node) (string? service) (address-family ai-family)) socket?)
             (((string? node) (string? service) (address-family ai-family) (socket-domain ai-socktype)) socket?)
             (((string? node) (string? service) (address-family ai-family) (socket-domain ai-socktype) (address-info ai-flags)) socket?)
             (((string? node) (string? service) (address-family ai-family) (socket-domain ai-socktype) (address-info ai-flags) (ip-protocol ai-protocol)) socket?))
  (desc . "Returns a client socket connected to an Internet address. The Internet address is identified by node and service. node and service must be string. Example value of node: \"localhost\" \"127.0.0.1\" Example value of service: \"http\" \"80\". The optional parameter may specify the created socket's behaviour.
If the optional argument(s) is omitted, then following flags should be used as default.
ai-family
    *af-inet*
ai-socktype
    *sock-stream*
ai-flags
    (socket-merge-flags *ai-v4mapped* *ai-addrconfig*)
ai-protocol
    *ipproto-ip*

The created socket may not be closed automatically so it is users' responsibility to close it explicitly."))
 ((name .  "make-server-socket")
  (signature case-lambda
             (((string? service)) socket?)
             (((string? service) (address-family ai-family)) socket?)
             (((string? service) (address-family ai-family) (socket-domain ai-socktype)) socket?)
             (((string? service) (address-family ai-family) (socket-domain ai-socktype)) socket?)
             (((string? service) (address-family ai-family) (socket-domain ai-socktype) (ip-protocol ai-protocol)) socket?))
  (desc . "Returns a server socket waiting for connection. The description of node argument is the same as make-client-socket. The optional parameter may specify the created socket's behaviour.
If the optional argument(s) is omitted, then following flags should be used as default.
ai-family
    *af-inet*
ai-socktype
    *sock-stream*
ai-protocol
    *ipproto-ip*

The created socket may not be closed automatically so it is users' responsibility to close it explicitly."))
 ((name . "socket?")
  (signature lambda (obj) boolean?)
  (tags predicate pure)
  (desc . "Returns #t if given object is socket object. Otherwise #f."))
 ((name . "socket-accept")
  (signature lambda ((socket? socket)) socket?)
  (desc . "Wait for an incoming connection request, and returns a fresh connected client socket."))
 ((name . "socket-send")
  (signature case-lambda
             (((socket? socket) (bytevector? bv)) integer?)
             (((socket? socket) (bytevector? bv) (message-type flags)) integer?))
  (desc . "Sends a binary data block to a socket and returns the sent data size. flags may specify the procedure's behaviour.
If the flags is omitted, the default value must be the result of following form;
(message-type none)"))
 ((name . "socket-recv")
  (signature case-lambda
             (((socket? socket) (integer? size)) bytevector?)
             (((socket? socket) (integer? size) (message-type flags)) bytevector?))
  (desc . "Receives a binary data block from a socket. If zero length bytevector is returned, it means the peer connection is closed. flags may specify the procedure's behaviour.
If the flags is omitted, the default value must be the result of following form;
(message-type none)"))
 ((name . "socket-shutdown")
  (signature lambda ((socket? socket) (shutdown-method how)) undefined)
  (desc . "Shutdowns a socket."))
 ((name . "socket-close")
  (signature lambda ((socket? socket)) undefined)
  (desc . "Closes a socket. The procedure should not shutdown the given socket. To shutdown a socket, socket-shutdown should be called explicitly."))
 ((group
    ((name . "socket-input-port")
     (signature lambda ((socket? socket)) input-port?))
    ((name . "socket-output-port")
     (signature lambda ((socket? socket)) output-port?)))
  (desc . "Returns a fresh binary input and output port associated with a socket, respectively. The port should not close underlying socket when it's closing."))
 ((name . "call-with-socket")
  (signature lambda ((socket? socket) (procedure? proc)) *)
  (subsigs
    (proc (lambda () *)))
  (desc . "Calls a given procedure with a given socket as an argument. If given proc returns then it returns the result of proc and socket will be automatically closed. If proc doesn't return then given socket won't be closed automatically. It's analogy of call-with-port."))
 ((name . "address-family")
  (signature syntax-rules ()
             ((_ name) address-family))
  (desc . "Returns proper address family from given name.
Implementation must support at least following names and must have the described behaviour.
inet
    Returns *af-inet*
inet6
    Returns *af-inet6*
unspec
    Returns *af-unspec*

Implementation may support more names such as unix or local or other names."))
 ((name . "address-info")
  (signature syntax-rules ()
             ((_ names ...) address-info))
  (desc . "Returns merged address info flags from given names.
Implementation must support at least following names and must have the described behaviour.
canoname
    Returns *ai-canonname*
numerichost
    Returns *ai-numerichost*
v4mapped
    Returns *ai-v4mapped*
all
    Returns *ai-all*
addrconfig
    Returns *ai-addrconfig*

Implementation may support more names."))
 ((name . "socket-domain")
  (signature syntax-rules ()
             ((_ name) socket-domain))
  (desc . "Returns socket domain flags from given name.
Implementation must support at least following names and must have the described behaviour.
stream
    Returns *sock-stream*
datagram
    Returns *sock-dgram*

Implementation may support more names."))
 ((name . "ip-protocol")
  (signature syntax-rules ()
             ((_ name) ip-protocol))
  (desc . "Returns ip-protocol flag from given name.
Implementation must support at least following names and must have the described behaviour.
ip
    Returns *ipproto-ip*
tcp
    Returns *ipproto-tcp*
udp
    Returns *ipproto-udp*

Implementation may support more names."))
 ((name . "message-type")
  (signature syntax-rules ()
             ((_ names ...) message-type))
  (desc . "Returns message type flag from given name. The flag can be used both socket-recv and socket-send.
Implementation must support at least following names and must have the described behaviour.
none
    Returns no flag.
peek
    Returns *msg-peek*
oob
    Returns *msg-oob*
wait-all
    Returns *msg-waitall*

Implementation may support more names."))
 ((name . "shutdown-method")
  (signature syntax-rules ()
             ((_ names ...) shutdown-method))
  (desc . "Returns shutdown method flags from given names.
Implementation must support at least following names and must have the described behaviour.
read
    Returns *shut-rd*
write
    Returns *shut-wr*

If shutdown-method is given both read and write, then it must return *shut-rdwr*"))
 ((name . "socket-merge-flags")
  (signature case-lambda
             (((address-info flags) ...) address-info)
             (((message-type flags) ...) message-type)
             (((shutdown-method flags) ...) shutdown-method))
  (desc . "Merges given flags and returns a new flag."))
 ((name . "socket-purge-flags")
  (signature case-lambda
             (((address-info base-flag) (address-info flags) ...) address-info)
             (((message-type base-flag) (message-type flags) ...) message-type)
             (((shutdown-method base-flag) (shutdown-method flags) ...) shutdown-method))
  (desc . "Removes flags from base-flag if exists and returns a new flag."))
 ((name . "*af-inet*")
  (signature value address-family)
  (desc . "Internet domain sockets for use with IPv4 addresses. This must behave the same as POSIX's AF_INET."))
 ((name . "*af-inet6*")
  (signature value address-family)
  (desc . "Internet domain sockets for use with IPv6 addresses. This must behave the same as POSIX's AF_INET6."))
 ((name . "*af-unspec*")
  (signature value address-family)
  (desc . "Unspecified. This must behave the same as POSIX's AF_UNSPEC."))
 ((name . "*sock-stream*")
  (signature value socket-domain)
  (desc . "Byte-stream socket. This must behave the same as POSIX's SOCK_STREAM."))
 ((name . "*sock-dgram*")
  (signature value socket-domain)
  (desc . "Datagram socket. This must behave the same as POSIX's SOCK_DGRAM."))
 ((name . "*ai-canonname*")
  (signature value address-info)
  (desc . "This must behave the same as POSIX's AI_CANONNAME."))
 ((name . "*ai-numerichost*")
  (signature value address-info)
  (desc . "This must behave the same as POSIX's AI_NUMERICHOST."))
 ((name . "*ai-v4mapped*")
  (signature value address-info)
  (desc . "This must behave the same as POSIX's AI_V4MAPPED."))
 ((name . "*ai-all*")
  (signature value address-info)
  (desc . "This must behave the same as POSIX's AI_ALL."))
 ((name . "*ai-addrconfig*")
  (signature value address-info)
  (desc . "This must behave the same as POSIX's AI_ADDRCONFIG."))
 ((name . "*ipproto-ip*")
  (signature value ip-protocol)
  (desc . "Internet protocol. This must behave the same as POSIX's IPPROTO_IP."))
 ((name . "*ipproto-tcp*")
  (signature value ip-protocol)
  (desc . "Transmission control protocol. This must behave the same as POSIX's IPPROTO_TCP."))
 ((name . "*ipproto-udp*")
  (signature value ip-protocol)
  (desc . "User datagram protocol. This must behave the same as POSIX's IPPROTO_UDP."))
 ((name . "*msg-peek*")
  (signature value message-type)
  (desc . "For socket-recv. Peeks at an incoming message. The data is treated as unread and the next socket-recv shall still return this data. This must behave the same as POSIX's MSG_PEEK."))
 ((name . "*msg-oob*")
  (signature value message-type)
  (desc . "For both socket-recv and socket-send. Requests/sends out-of-band data. This must behave the same as POSIX's MSG_OOB."))
 ((name . "*msg-waitall*")
  (signature value message-type)
  (desc . "For socket-recv. On sockets created with *sock-stream* flag, this requests the procedure block until the full amount of data ban be returned. This must behave the same as POSIX's MSG_WAITALL."))
 ((name . "*shut-rd*")
  (signature value shutdown-method)
  (desc . "Disables further receive operation. This must behave the same as POSIX's SHUT_RD."))
 ((name . "*shut-wr*")
  (signature value shutdown-method)
  (desc . "Disables further send operations. This must behave the same as POSIX's SHUT_WR."))
 ((name . "*shut-rdwr*")
  (signature value shutdown-method)
  (desc . "Disables further send and receive operations. This must behave the same as POSIX's SHUT_RDWR.")))
