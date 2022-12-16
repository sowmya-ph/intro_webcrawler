%%%-------------------------------------------------------------------
%% @doc intro_webcrawler public API
%% @end
%%%-------------------------------------------------------------------

-module(intro_webcrawler_app).

-behaviour(application).

-export([start/2, stop/1, test_htmerl/0, crawl_all/2]).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(LOG_FILE, "log.txt").
-define(debug_log(X), file:write_file(?LOG_FILE, io_lib:format("~p~n", [X]), [append])).
-endif.

-record(webcrawler_words, {character, word}).
-record(webcrawler_urls, {word, urls=[]}).

install(Nodes) ->
ok = mnesia:create_schema(Nodes),
rpc:multicall(Nodes, application, start, [mnesia]),
mnesia:create_table(intro_webcrawler_words,
[{attributes, record_info(fields, webcrawler_words)},
{disc_copies, Nodes}]),
mnesia:create_table(webcrawler_urls,
[{attributes, record_info(fields, webcrawler_urls)},
{disc_copies, Nodes},
{type, bag}]),
rpc:multicall(Nodes, application, stop, [mnesia]).

start(_StartType, _StartArgs) ->
    application:start(mochiweb),
    mnesia:wait_for_tables([webcrawler_words, webcrawler_urls], 5000),
    intro_webcrawler_sup:start_link().

crawl_all(SeedURL, Depth) ->
    %init
    inets:start(),
    ssl:start(),
    % file:write_file(?LOG_FILE, ""),
    
    % Init with seed pages
    % TODO: pull these out into a config file
    crawl([SeedURL], Depth).

crawl([], _) -> done;
crawl(_, Depth) when Depth < 1 -> done;
crawl(Url, Depth) -> case httpc:request(Url) of
                         {ok, {{_, 200, _}, _, Body}} -> 
                            % Parse the HTML using mochiweb
                            HTML_Tree = mochiweb_html:parse(Body),
                            HTML_Tokens = mochiweb_html:to_tokens(HTML_Tree),

                            % Parse the words and hrefs out from tokens
                            get_words_links(HTML_Tokens, [], [])

                        
                            % todo: add error handling
                     end.
                     % todo: add parsed links to Url queue and decrement Depth

get_words_links([], Words, Links) -> {Words, Links};

get_words_links([Token|Tail], Words, Links) ->
    case Token of
        {data, WordStr, false} -> get_words_links(Tail,  Words ++ string:tokens(erlang:binary_to_list(WordStr), [$\s]), Links);
        {start_tag,<<"a">>, [{<<"href">>, URL}], false} -> get_words_links(Tail,  Words, Links ++ [erlang:binary_to_list(URL)]);
        _ -> get_words_links(Tail, Words, Links)
    end.

test_htmerl() ->
    htmerl:simple(<<"<!DOCTYPE html><html><body>Hello</body></html>">>).

stop(_State) ->
    ok.

%% internal functions
