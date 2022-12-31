-module('day5-solution').
-import(lists, [droplast/1]).
-export([run/0]).
-on_load(run/0).

read_configuration(StringConfiguration) ->
    InitialConfiguration = #{
                             0 => [],
                             1 => [],
                             2 => [],
                             3 => [],
                             4 => [],
                             5 => [],
                             6 => [],
                             7 => [],
                             8 => []
                            },

    CrateLines = lists:droplast(string:split(StringConfiguration, "\n", all)),

    lists:foldl(
      fun(CrateLine, Accumulator) ->
              {match, RawMatchesOnLine} = re:run(CrateLine, "[[:alpha:]]", [global]),
              MatchesOnLine = lists:flatten(RawMatchesOnLine),

              lists:foldl(
                fun({Idx, _}, Configuration) ->
                        ConfigIdx = (Idx - 1) div 4,
                        Crate = binary:at(CrateLine, Idx),
                        maps:update_with(ConfigIdx,
                                         fun(List) -> lists:append(List, [Crate]) end,
                                         Configuration)
                end,
                Accumulator,
                MatchesOnLine)
      end,
      InitialConfiguration,
      CrateLines).

parse_instruction(InstrLine) ->
    {match, RawMatchesOnLine} = re:run(InstrLine, <<"(\\d+)">>, [global]),
    MatchesOnLine = lists:uniq(lists:flatten(RawMatchesOnLine)),

    lists:foldr(
      fun({StartPos, Length}, Accumulator) ->
              NumberStr = string:slice(InstrLine, StartPos, Length),
              {Number, _} = string:to_integer(NumberStr),
              erlang:insert_element(1, Accumulator, Number)
      end,
      {},
      MatchesOnLine).

move_crates(Instructions, InitialConfiguration, ShouldReverse) ->
    lists:foldl(
      fun({CrateCount, From, To}, Configuration) ->
              CratesToMove = lists:sublist(maps:get(From-1, Configuration), CrateCount),

              TempConfiguration = maps:update_with(
                                    From-1,
                                    fun(Crates) ->
                                            lists:sublist(Crates, CrateCount+1, length(Crates))
                                    end,
                                    Configuration),

              maps:update_with(
                To-1,
                fun(Crates) ->
                        case ShouldReverse == true of
                            true -> lists:append(lists:reverse(CratesToMove), Crates);
                            false -> lists:append(CratesToMove, Crates)
                        end
                end,
                TempConfiguration)
      end,
      InitialConfiguration,
      Instructions).

run() ->
    {ok, RawInput} = file:read_file("day5-input.txt"),
    [Configuration, Instructions] = string:split(RawInput, "\n\n"),

    InitialConfiguration = read_configuration(Configuration),
    ParsedInstructions = lists:map(fun parse_instruction/1, string:split(string:trim(Instructions), "\n", all)),

    FinalConfiguration1 = move_crates(ParsedInstructions, InitialConfiguration, true),
    Answer1 = lists:map(fun(Crates) -> hd(Crates) end, maps:values(FinalConfiguration1)),
    io:format("Answer 1: ~p~n", [Answer1]), %% TWSGQHNHL

    FinalConfiguration2 = move_crates(ParsedInstructions, InitialConfiguration, false),
    Answer2 = lists:map(fun(Crates) -> hd(Crates) end, maps:values(FinalConfiguration2)),
    io:format("Answer 2: ~p~n", [Answer2]), %% JNRSCDWPP

    ok.
