%%%-------------------------------------------------------------------
%% @doc intro_webcrawler public API
%% @end
%%%-------------------------------------------------------------------

-module(intro_webcrawler_app).

-behaviour(application).

-export([start/2, stop/1, test_htmerl/0, 
         crawl_all/2]).

-define(DEBUG, true).

-ifdef(DEBUG).
-define(LOG_FILE, "log.txt").
-define(debug_log(X), file:write_file(?LOG_FILE, io_lib:format("~p~n", [X]), [append])).
-endif.

start(_StartType, _StartArgs) ->
    application:start(mochiweb).
    % init_mnsesia(),
    % intro_webcrawler_sup:start_link().

crawl_all(SeedURL, Depth) ->
    %init
    inets:start(),
    ssl:start(),
    % file:write_file(?LOG_FILE, ""),
    
    % Init with seed pages
    % TODO: pull these out into a config file
    crawl([SeedURL], Depth).

get_words_links([], CurrUrl, Urls) -> Urls;

get_words_links([Token|Tail], CurrUrl, Urls) ->
    io:format("~s~n", [Token]),
    get_words_links(Tail, CurrUrl, Urls).

    %case Token of
    %    {data, <<"p">>++WordStr, false} -> add_to_db(WordStr, Url), 
    %                              get_words_links(Tail, CurrUrl, Urls);
    %    {start_tag,<<"a">>, [{<<"href">>, "https://"URL}], false} -> add_to_db(Url),
    %                                                                  get_words_links(Tail,  Urls ++ [erlang:binary_to_list(URL)]);
    %    _ -> get_words_links(Tail, CurrUrl, Urls)
    %end.



crawl([], _) -> done;
crawl(_, Depth) when Depth < 1 -> done;
crawl([Url | Urls], Depth) -> case httpc:request(Url) of
                         {ok, {{_, 200, _}, _, Body}} -> 
                            % Parse the HTML using mochiweb
                            HTML_Tree = mochiweb_html:parse(Body),
                            HTML_Tokens = mochiweb_html:to_tokens(HTML_Tree),

                            % Parse the words and hrefs out from tokens
                            crawl(lists:append(Urls, get_words_links(HTML_Tokens, Url, Urls)), Depth - 1)

                        
                            % todo: add error handling
                     end.
                     % todo: add parsed links to Url queue and decrement Depth
test_htmerl() ->
    htmerl:simple(<<"<!DOCTYPE html><html><body>Hello</body></html>">>).

stop(_State) ->
    ok.

%% internal functions
