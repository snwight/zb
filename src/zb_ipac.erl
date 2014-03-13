%%%-------------------------------------------------------------------
%%% @author Steve Wight <northwight@gmail.com>
%%% @doc
%%%     Reads IPAC format table representation -
%%%     very common container for astronomical data 
%%% @end
%%% Created :  7 Mar 2014 by Steve Wight
%%%-------------------------------------------------------------------
-module(zb_ipac).

-export([read/1]).

-export([zip4/4, parse_hdr/1, parse_hdr/2,  parse_ipac/1]).

read(IPACFile) ->
    case file:read_file(IPACFile) of
	{ok, Contents} ->
	    %% this is insanely unscalable
	    parse_ipac(binary_to_list(Contents));
	{error, Reason} ->
	    io:format("Error ~p accessing ~s~n", [Reason, IPACFile]),
	    []
    end.

parse_ipac(Contents) ->
    {HdrTuples, Body} = parse_hdr(Contents),
    DataTuples = parse_body(HdrTuples, Body),
    {HdrTuples, DataTuples}.

parse_hdr(Contents) ->
    %% break up [ENTIRE] file into lines
    %% strip comments off head of file
    %% collect pipe-and-tab hdr lines and body of data 
    Lines = string:tokens(Contents, "\n"),
    Rest = lists:dropwhile(fun("\\"++_) -> true; (_) -> false end, Lines),
    {Hdr, Body} = lists:splitwith(fun("|"++_) -> true; (_) -> false end, Rest),
    {parse_hdr(Hdr, []), Body}.

parse_hdr([], HdrInfo) ->
    zipit(lists:reverse(HdrInfo));
parse_hdr([H="|"++_ | T], []) ->
    Tokens = string:tokens(H, "|"),
    Names = lists:map(fun(Tok) -> string:strip(Tok, both, $\ ) end, Tokens),
    parse_hdr(T, [Names]);
parse_hdr([H="|"++_ | T], HdrInfo) when length(HdrInfo) == 1 ->
    Tokens = string:tokens(H, "|"),
    Types = lists:map(fun(Tok) -> string:strip(Tok, both,  $\ ) end, Tokens),
    parse_hdr(T, [Types | HdrInfo]);
parse_hdr([H="|"++_ | T], HdrInfo) when length(HdrInfo) == 2 ->
    Tokens = string:tokens(H, "|"),
    Units = lists:map(fun(Tok) -> string:strip(Tok, both,  $\ ) end, Tokens),
    parse_hdr(T, [Units | HdrInfo]);
parse_hdr([H="|"++_ | T], HdrInfo) when length(HdrInfo) == 3 ->
    Tokens = string:tokens(H, "|"),
    Defaults = lists:map(fun(Tok) -> string:strip(Tok, both, $\ ) end, Tokens),
    parse_hdr(T, [Defaults | HdrInfo]).

parse_body(HdrTuples, Body) ->
    %% {"ra","double","deg","null"},    
    %% {"dec","double","deg","null"},
    %% {"clon","char",[],"null"},
    %% {"clat","char",[],"null"},
    lists:map(
      fun(Row) ->
	      Merged = lists:zip(HdrTuples, Row),
	      lists:map(
		fun({{N, T, U, D}, E}) -> {"column", N, "type", T, "units", U, "default", D, "data", E};
		   ({{N, T, U}, E}) -> {"column", N, "type", T, "units", U, "data", E};
		   ({{N, T}, E}) -> {"column", N, "type", T, "data", E};
		   ({{N}, E}) -> {"column", N, "data", E}
		end, Merged)
      end, Body).

zipit([T1])          -> T1;
zipit([T1, T2])      -> lists:zip(T1, T2);
zipit([T1, T2, T3])  -> lists:zip3(T1, T2, T3);
zipit([T1,T2,T3,T4]) -> zip4(T1, T2, T3, T4).

zip4(T1, T2, T3, T4) ->
    zip4(T1, T2, T3, T4, []).

zip4([], [], [], [], Tuples) ->
    lists:reverse(Tuples);
zip4([H1|T1], [H2|T2], [H3|T3], [H4|T4], Tuples) ->
    zip4(T1, T2, T3, T4, [{H1,H2,H3,H4}|Tuples]).

