-module(http).
-export([stressTest/3,
		 getProxy/2, 
		 get/1,
		 timer/3]).

stressTest(Url, Wait, N) when Wait > 0 -> 
	spawn(?MODULE, timer, [100, self(), N]),
	This = self(),
	recv({0,0, N}),
	io:format("timer start, pid ~p!~n", [self()]);
stressTest(_, _,_) -> error.

timer(T, Who, N) ->
	receive
		after T ->
			Who ! {stats}
	end,
	timer(T, Who, N).

recv(Stats) ->
	{Active, Closed, N} = Stats,
	receive
		{stats} -> io:format("received stats: "),
				   if Active < N, N > 0 ->
						  io:format("+1~n"),
						  recv({Active+1, Closed, N});
					true ->
						io:format("-1~n"),
						recv({Active-1, Closed+1, 0})
				   end
	end.

get(Url) ->
	{http, Host, Port, _}=parse(Url),
	getProxy(Url, {Host, Port}).

getProxy(Url, {ProxyHost, ProxyPort}) ->
	process_flag(trap_exit, true),
	BeforeConnectTime = nowMicroSeconds(),
	{ok, Socket} = connect(ProxyHost, ProxyPort),
	AfterConnectTime = nowMicroSeconds(),
	ok = send(Socket, Url),
	Response = recvResponse(),
	Hdr = recvHdr([]),
	Body = recvBody(),
	AfterReceiveTime = nowMicroSeconds(),

	{
		{ok},
		{connect_time,micro_second, AfterConnectTime-BeforeConnectTime},
		{total_time, micro_second, AfterReceiveTime-BeforeConnectTime},
		{response, Response},
		{headers, Hdr},
		{body, Body}
	}.

connect(Host, Port) ->
	{ok, Socket} = gen_tcp:connect(Host, Port, [{active, once}, {packet, http}]),
	{ok, Socket}.

send(Socket, Url) ->
	Request = createRequest(Url),
	ok = gen_tcp:send(Socket, Request),
	ok.

createRequest(Url) ->
	{http, Host, Port, Path}=parse(Url),
	L1 = "GET "++Path++" HTTP/1.0\r\n",
	L2 = L1++"Host: "++Host++":"++integer_to_list(Port)++"\r\n",
	L2++"Accept: "++" */*"++"\r\n\r\n".

recvResponse() ->
	receive
		{http, Socket, HttpPacket} ->
			case HttpPacket of
			{http_response, Version, Code, String} ->
				inet:setopts(Socket,[{active, once}, {packet, httph}]),
				{Version, Code, String}
			end
	end.

recvHdr(L) ->
    receive
		{http, Socket, HttpPacket} ->
			case HttpPacket of
			{http_response, Version, Code, String} ->
				inet:setopts(Socket,[{active, once}, {packet, httph}]),
				R = {Version, Code, String},
				recvHdr([R|L]);
			{http_header, _, Header, _, Value} ->
				inet:setopts(Socket,[{active, once}, {packet, httph}]),
				H = {Header, Value},
				recvHdr([H|L]);
			http_eoh ->
				inet:setopts(Socket,[{active, once}, {packet, raw}]),
				lists:reverse(L)
			end
	end.

recvBody() ->
    receive
		{tcp, _, Body} ->
			{Body}
	end.

parse([$h,$t,$t,$p,$:,$/,$/|T]) ->  parse({http,T});
parse({http,X}) ->
   case string:chr(X, $/) of
   0 ->
       %% not terminated by "/"
       parse({http, X ++ "/"});
   N ->
       Host = string:substr(X, 1, N-1),
       File = string:substr(X, N, length(X)),
       case string:chr(Host, $:) of
       0 ->
           %% no colon
           Port = 80,
           {http, Host, Port, File};
       M ->
           Site = string:substr(Host,1,M-1),
           case (catch list_to_integer(
                 string:substr(Host, M+1, length(Host)))) of
           {'EXIT', _} ->
               {http, Site, 80, File};
           Port ->
               {http, Site, Port, File}
           end
       end
   end;

 parse(_X) ->  {error, unknown_url_type}.

nowMicroSeconds() ->
	{MegaSecs,Secs,MicroSecs} = now(),
	(MegaSecs*1000000 + Secs)*1000000 + MicroSecs.