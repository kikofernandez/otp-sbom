-module(reuse).
%% SPDX-License-Identifier: Apache-2.0

-export([main/0, main/1, reuse_ignore/0]).

main() ->
    Filename = 'scan-result-classified.json',
    main(Filename).

main(Input) ->
    {ok, Bin} = file:read_file(Input),
    Json = json:decode(Bin),
    maps:foreach(fun (License, ListOfBin) when License =/= ~"NONE" ->
                         io:format("~s~n", [License]),
                         Vs = lists:map(fun erlang:binary_to_list/1, ListOfBin),
                         %% filter_erlang(erlang:binary_to_list(License), Vs);
                         %% filter_md(erlang:binary_to_list(License), Vs);
                         filter_c(erlang:binary_to_list(License), Vs);
                     (~"NONE", _) ->
                         io:format("~s~n", [~"NONE"]),
                         skip
                 end, Json).

reuse_ignore() ->
    Filename = 'scan-result-classified.json',
    {ok, Bin} = file:read_file(Filename),
    Json = json:decode(Bin),
    extensions_ignore(),
    ok.

extensions_ignore() ->
    [".beam", ".boot", ".version", "OTP_VERSION", ".mid", "COPYRIGHT",
     ".sgml", ".cocci", "LICENSE", "LICENCE", "erlc", "erl", ".json",
     ".pem", ".gif", ".fig", ".svg", ".odg", ".png", ".log", ".gz",
     "tgz", ".stub", ".jpg", ".tar", ".pub"].

extension() ->
    [".erl", ".hrl", ".c", ".cpp", ".h", ".md", ".conf", ".sh",
     "Makefile", ".escript", "mk.in", ".mk", ".el", ".erlsrc",
     ".yrl", ".c_src", ".app.src", ".appup.src", ".ac", ".src", ".d",
     ".systemtap", ".java", "Makefile.first", "Makefile.ca", ".dia", ".pl",
    ".asn", "GNUmakefile", ".app", ".appup", ".sed", ".html", ".xml",
     ".py", ".conf.template", "Emakefile", "README", ".tab", ".tla",
     "configure", ".sub", ".guess", ".exs", ".js", ".xsd", ".xsl", ".dtd",
     ".css", ".asn1"].

%% dia -> ;;
%% sed -> #
%% .conf.template -> #
%% README -> #
%% .txt -> #
%% .tab -> //
%% .sub -> #
%% .guess -> #
%% .asn1 -> --

filter_erlang(License, Vs) ->
    Vs1 = lists:filter(fun(X) when X =/= "erts/test/erlc_SUITE_data/src/😀/erl_test_unicode.erl" orelse
                                   X =/= "erts/test/erlc_SUITE_data/src/ð/erl_test_unicode.erl" orelse
                                   X =/= "lib/ssh/src/ssh.erl"->
                              case lists:reverse(X) of
                                  "lre."++_ ->
                                      true;
                                  "lrh."++_ ->
                                      true;
                                  _ ->
                                      false
                              end;
                         (_) ->
                              false
                      end, Vs),
    %% Group files in bunch of 50s for reuse to not crash
    LL = groups_by(lists:sort(Vs1), 50),
    add_license(License, LL).

groups_by(Ls, N) ->
    {_, R} = lists:foldl(fun(File, {X, Acc}) when X == N ->
                                 {1, [ [File] | Acc ]};
                            (File, {X, []}) ->
                                 {X+1, [ [File] ]};
                            (File, {X, [Xs | Acc]}) ->
                                 {X+1, [[File | Xs] | Acc]}
                         end, {1, []}, Ls),
    R.

add_license(License, LL) ->
    lists:foreach(fun (Files0) ->
                          Command = "cd otp && reuse annotate --no-replace --license \""++ License ++ "\" " ++ lists:join(" ", Files0),
                          Command1 = Command ++ " && dos2unix " ++ lists:join(" ", Files0),
                          Result = os:cmd(Command1),
                          io:format("~s~n", [erlang:list_to_binary(Result)])
                  end, LL).


%% reuse annotate --no-replace --license "Apache-2.0" lib/ssh/examples/ssh_sample_cli.erl lib/ssh/examples/ssh_device.erl lib/snmp/test/test_config/snmp_test_config.erl lib/snmp/test/test2.erl lib/snmp/test/test1.erl lib/snmp/test/snmp_to_snmpnet_SUITE.erl lib/snmp/test/snmp_test_sys_monitor.erl lib/snmp/test/snmp_test_suite.erl lib/snmp/test/snmp_test_server.erl lib/snmp/test/snmp_test_mgr_misc.erl

filter_md(License, Vs) ->
    Vs1 = lists:filter(fun(X) ->
                         case lists:reverse(X) of
                             "dm."++_ ->
                                 true;
                             _ ->
                                 false
                         end
                 end, Vs),
    %% Group files in bunch of 50s for reuse to not crash
    LL = groups_by(lists:sort(Vs1), 1),
    add_license(License, LL).


filter_c(License, Vs) ->
    Vs1 = lists:filter(fun(X) ->
                         case lists:reverse(X) of
                             %% "c."++_ ->
                             %%     true;
                             %% "ppc."++_ ->
                             %%     true;
                             %% "h."++_ ->
                             %%     true;
                             "pph."++_ ->
                                 true;
                             _ ->
                                 false
                         end
                      end, Vs),
    %% Group files in bunch of 50s for reuse to not crash
    LL = groups_by(lists:sort(Vs1), 1),
    add_license(License, LL).
