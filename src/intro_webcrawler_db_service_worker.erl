-module(intro_webcrawler_db_service_worker).
-behavior(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([add_word/3, lookup_word/2, lookup_character/2, add_url/1, install/1, add_words_to_db/2 ]).

-record(webcrawler_words, {character, word}).
-record(webcrawler_urls, {word, url}).
-record(webcrawler_wordcounts, {character, word_count}).
-record(webcrawler_allurls, {url}).

%%% Server functions
init([Nodes]) -> 
    %% To know when the parent shuts down
    process_flag(trap_exit, true),
    install(Nodes), 
    mnesia:wait_for_tables([webcrawler_words, webcrawler_urls, webcrawler_allurls], 5000),
    {ok, []}. %% no treatment of info here!
 
% all these call could have been made casts (async) but 
% we might want to add error handling in clients
handle_call({add_word, Character, Word, Url}, _From, _State) ->
    F = fun() ->
                % Write the char and word into the words table, overwrites are ok
                mnesia:write(#webcrawler_words{character=Character,
                                               word=Word}),
                % Write to the URLs table 
                mnesia:write(#webcrawler_urls{word=Word, url=Url})
    end,
    {reply, mnesia:activity(transaction, F)};

handle_call({add_url, Url}, _From, _State) ->
    F = fun() ->
                mnesia:write(#webcrawler_allurls{url=Url})
        end,
    {reply, mnesia:activity(transaction, F), []};

handle_call({lookup_character, Character, Limit}, _From, _State) ->
    F = fun() ->
                {Res, _metadata} = mnesia:select(webcrawler_words, [{#webcrawler_words{character = Character, word = '$1'}, [], ['$1']}], Limit, read),
                lists:foldl(fun(W, Tail) ->
                                                   lists:append([{W, lookup_word(W, Limit)}], Tail) end, [], Res)
        end,
    {reply, mnesia:activity(transaction, F), []};

% TODO: Handle limit
handle_call({lookup_word, Word, Limit}, _From, _State) ->
    F = fun() ->
                Res = mnesia:read({webcrawler_urls, Word}),
                case Res of
                    [] -> undefined;
                    ResList -> 
                        % From the query result records, extract URLs and create a URL list
                        lists:foldl(fun(#webcrawler_urls{word=_, url=U}, OldUrls) -> 
                                            lists:append([U], OldUrls) end, [], ResList)
                end
        end,
    {reply, mnesia:activity(transaction, F), []};

handle_call(terminate, _From, _State) ->
    {stop, normal, ok, []}.

handle_cast(_Message, S) ->
    {noreply, S}.

handle_info(Msg, _State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, []}.

terminate(normal, _State) ->
    application:stop(mnesia),
    io:format("DB server was terminated normally.~n");
terminate(_Reason, _) ->
io:format("Process terminated for reason ~n").

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

% Creates a gen_server process as part of a supervision tree. This function is to be called, directly or indirectly, by the supervisor. For example, it ensures that the gen_server process is linked to the supervisor.
% The gen_server process calls Module:init/1 to initialize. To ensure a synchronized startup procedure, start_link/3,4 does not return until Module:init/1 has returned.
start_link(Nodes) ->
gen_server:start_link({local, intro_webcrawler_db_service_worker}, intro_webcrawler_db_service_worker, [Nodes], []).


%%% Private functions
install(Nodes) ->
    mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(webcrawler_words,
                        [{attributes, record_info(fields, webcrawler_words)},
                         {disc_copies, Nodes},
                         {type, bag}]),
    mnesia:create_table(webcrawler_urls,
                        [{attributes, record_info(fields, webcrawler_urls)},
                         {disc_copies, Nodes},
                         {type, bag}]),
    mnesia:create_table(webcrawler_wordcounts,
                        [{attributes, record_info(fields, webcrawler_wordcounts)},
                         {disc_copies, Nodes},
                         {type, set}]),
    mnesia:create_table(webcrawler_allurls,
                        [{attributes, record_info(fields, webcrawler_allurls)},
                         {disc_copies, Nodes},
                         {type, set}]),
    rpc:multicall(Nodes, application, stop, [mnesia]).

add_word(Character, Word, Url) ->
    gen_server:call(intro_webcrawler_db_service_worker, {add_word, Character, Word, Url}).

lookup_word(Word, Limit) ->
    gen_server:call(intro_webcrawler_db_service_worker, {lookup_word, Word, Limit}).

add_url(Url) ->
    gen_server:call(intro_webcrawler_db_service_worker, {add_url, Url}).

% given a character, it returns upto Limit words that start with the character and for each word, a list of URLs 
lookup_character(Character, Limit) ->
    gen_server:call(intro_webcrawler_db_service_worker, {lookup_character, Character, Limit}).

add_words_to_db([], _) -> ok;
add_words_to_db([Word|Words], URL) ->
    intro_webcrawler_db_service_worker:add_word(lists:sublist(Word, 1, 4), Word, URL),
    add_words_to_db(Words, URL).



