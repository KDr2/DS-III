module EchoServer

using JSON

const PORT = 9876

let stop_flag = false
    global serve, async_serve
    global stop_serve

    function serve()
        server = listen(Base.IPv4(0,0,0,0),uint16(PORT))
        while !stop_flag
            client = Base.accept(server)
            @async client_handler(client)
        end
    end

    async_serve() = @async serve()
    stop_serve() = (stop_flag = true)
end

function client_handler(sock)
    try
        if eof(sock) return end #!!! for issue #2684

        p = Array(Uint8, 6)

        while !eof(sock)
            p = read(sock, p)
            write(sock, p)
        end
    catch e
        print("error: $(e)")
    end
end

end # end module
