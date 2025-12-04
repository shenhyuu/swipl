:- encoding(utf8).

:- use_module(library(random)).
:- use_module(library(readutil)).

:- dynamic role/2.
:- dynamic location/2.
:- dynamic alive/1.
:- dynamic dead/1.
:- dynamic task_state/4.
:- dynamic round_number/1.
:- dynamic last_meeting_round/1.
:- dynamic total_tasks_remaining/1.
:- dynamic kill_cooldown/1.
:- dynamic scout_cooldown/1.
:- dynamic corpse/2.
:- dynamic inspected/2.
:- dynamic known_fox/1.
:- dynamic not_in_task_counter/2.
:- dynamic alone_counter/2.

%% Static task definitions
base_task(collect_food, '收集食材', kitchen, 2).
base_task(repair_wiring, '修理电路', living_room, 3).
base_task(clean_vent, '清理通风管', bedroom, 2).

%% Map connectivity
connected(kitchen, living_room).
connected(living_room, kitchen).
connected(living_room, bathroom).
connected(bathroom, living_room).
connected(living_room, bedroom).
connected(bedroom, living_room).
connected(bedroom, balcony).
connected(balcony, bedroom).

scenes([kitchen, living_room, bathroom, bedroom, balcony]).

%% Entry point
start_game :-
    setup_game,
    game_loop.

setup_game :-
    retractall(role(_, _)),
    retractall(location(_, _)),
    retractall(alive(_)),
    retractall(dead(_)),
    retractall(task_state(_, _, _, _)),
    retractall(round_number(_)),
    retractall(last_meeting_round(_)),
    retractall(total_tasks_remaining(_)),
    retractall(kill_cooldown(_)),
    retractall(scout_cooldown(_)),
    retractall(corpse(_, _)),
    retractall(inspected(_, _)),
    retractall(known_fox(_)),
    retractall(not_in_task_counter(_, _)),
    retractall(alone_counter(_, _)),
    init_roles,
    init_locations,
    init_tasks,
    assertz(round_number(0)),
    assertz(last_meeting_round(0)),
    assertz(kill_cooldown(0)),
    assertz(scout_cooldown(0)),
    format("游戏初始化完成。~n"),
    show_status.

init_roles :-
    assertz(role(fox, fox)),
    assertz(role(rabbit1, rabbit)),
    assertz(role(rabbit2, rabbit)),
    assertz(role(rabbit3, rabbit)),
    assertz(role(rabbit4, rabbit)),
    assertz(role(detective, detective)),
    forall(role(Name, _), assertz(alive(Name))).

init_locations :-
    assertz(location(fox, living_room)),
    assertz(location(rabbit1, kitchen)),
    assertz(location(rabbit2, bathroom)),
    assertz(location(rabbit3, bedroom)),
    assertz(location(rabbit4, balcony)),
    assertz(location(detective, living_room)),
    forall(role(Name, _), (
        assertz(not_in_task_counter(Name, 0)),
        assertz(alone_counter(Name, 0))
    )).

init_tasks :-
    findall(Id, base_task(Id, _, _, _), TaskIds),
    length(TaskIds, Total),
    assertz(total_tasks_remaining(Total)),
    forall(base_task(Id, _, Scene, Duration),
        assertz(task_state(Id, free, Duration, none))
    ).

%% Game loop

game_loop :-
    round_number(R),
    R1 is R + 1,
    retract(round_number(R)),
    assertz(round_number(R1)),
    format("\n===== 第 ~d 轮 =====~n", [R1]),
    alive_summary,
    normal_rabbit_actions,
    detective_action,
    player_action,
    end_round_updates,
    maybe_meeting,
    ( victory(Outcome) -> format("游戏结束：~w 获胜！~n", [Outcome])
    ; game_loop).

alive_summary :-
    findall(Name-Type-Loc, (alive(Name), role(Name, Type), location(Name, Loc)), List),
    forall(member(N-T-L, List), format("~w (~w) 位于 ~w。~n", [N, T, L])).

%% Normal rabbit actions
normal_rabbit_actions :-
    forall((alive(Name), role(Name, rabbit)), rabbit_turn(Name)).

rabbit_turn(Name) :-
    location(Name, Scene),
    ( corpse(Scene, _) ->
        format("普通兔 ~w 发现尸体，停止行动。~n", [Name])
    ; ( task_in_scene(Scene, TaskId, Status, Occupant), Status \= completed ->
            ( Status = free ->
                occupy_task(Name, TaskId, Scene)
            ; Occupant = Name ->
                format("普通兔 ~w 继续占用任务 ~w。~n", [Name, TaskId])
            ; format("普通兔 ~w 看到任务 ~w 已被占用。~n", [Name, TaskId])
            )
      ; move_to_nearest_task(Name)
      )
    ).

occupy_task(Name, TaskId, Scene) :-
    retract(task_state(TaskId, free, Rem, _)),
    assertz(task_state(TaskId, occupied, Rem, Name)),
    format("普通兔 ~w 在 ~w 占用任务 ~w。~n", [Name, Scene, TaskId]).

move_to_nearest_task(Name) :-
    findall(TaskId-Scene, (task_state(TaskId, free, _, _), base_task(TaskId, _, Scene, _)), Tasks),
    ( Tasks = [] ->
        random_move(Name)
    ; location(Name, Current),
      nearest_scene(Current, Tasks, TargetScene, _),
      ( Current = TargetScene -> true
      ; step_towards(Current, TargetScene, Next),
        move_character(Name, Next),
        format("普通兔 ~w 向 ~w 移动。~n", [Name, Next])
      )
    ).

random_move(Name) :-
    location(Name, Scene),
    findall(Adj, connected(Scene, Adj), Adjs),
    ( Adjs = [] -> true
    ; random_member(Next, Adjs),
      move_character(Name, Next),
      format("普通兔 ~w 随机移动到 ~w。~n", [Name, Next])
    ).

%% Detective actions
detective_action :-
    ( alive(detective) -> detective_turn ; true ).



detective_turn :-
    location(detective, Scene),
    ( corpse(Scene, _) ->
        format("侦探兔发现尸体，停止行动。~n")
    ; scout_cooldown(0) ->
        ( select_inspection_target(Scene, Target) ->
            perform_inspection(Target)
        ; move_towards_unchecked
        )
    ; move_towards_unchecked
    ).

select_inspection_target(Scene, Target) :-
    alive(Target), Target \= detective,
    \+ inspected(Target, _),
    location(Target, Scene),
    !.

perform_inspection(Target) :-
    role(Target, Role),
    assertz(inspected(Target, Role)),
    ( Role = fox -> assertz(known_fox(Target)), format("侦探兔查出 ~w 是狐狸！~n", [Target])
    ; format("侦探兔查验 ~w，身份为兔。~n", [Target])
    ),
    set_scout_cooldown(2).

move_towards_unchecked :-
    findall(Target-Loc, (alive(Target), Target \= detective, \+ inspected(Target, _), location(Target, Loc)), Targets),
    ( Targets = [] -> random_move(detective)
    ; location(detective, Current),
      nearest_scene(Current, Targets, TargetScene, _),
      ( Current = TargetScene -> format("侦探兔等待机会。~n")
      ; step_towards(Current, TargetScene, Next),
        move_character(detective, Next),
        format("侦探兔移动到 ~w。~n", [Next])
      )
    ).

set_scout_cooldown(N) :-
    retractall(scout_cooldown(_)),
    assertz(scout_cooldown(N)).

%% Player action
player_action :-
    ( alive(fox) -> player_turn ; format("狐狸已死亡，跳过玩家回合。~n") ).

player_turn :-
    location(fox, Scene),
    format("\n你在 ~w。选择操作：(m)移动 (k)杀兔 (w)等待 > ", [Scene]),
    read_line_to_string(user_input, choice),
    handle_player_choice(choice).

handle_player_choice("m") :-
    location(fox, Scene),
    findall(Adj, connected(Scene, Adj), Adjs),
    ( Adjs = [] -> format("没有可移动的相邻场景。~n")
    ; format("可前往: ~w~n", [Adjs]),
      read_line_to_string(user_input, DestStr),
      atom_string(Dest, DestStr),
      ( member(Dest, Adjs) ->
            move_character(fox, Dest),
            format("移动到 ~w。~n", [Dest])
        ; format("输入无效，保持原地。~n")
      )
    ),
    kill_prompt.
handle_player_choice("k") :-
    kill_prompt.
handle_player_choice("w") :-
    format("你选择等待。~n"), kill_prompt.
handle_player_choice(_) :-
    format("输入无效，默认等待。~n"), kill_prompt.

kill_prompt :-
    kill_cooldown(CD),
    ( CD > 0 -> format("杀兔技能冷却中，还需 ~d 轮。~n", [CD])
    ; location(fox, Scene),
      findall(Rabbit, (alive(Rabbit), role(Rabbit, Type), Type \= fox, location(Rabbit, Scene)), Rabbits),
      ( Rabbits = [] -> format("哎呀，兔子都躲猫猫了！下次试试移动？~n")
      ; format("可击杀的兔子: ~w~n", [Rabbits]),
        read_line_to_string(user_input, TargetStr),
        atom_string(Target, TargetStr),
        ( member(Target, Rabbits) ->
            eliminate(Target, kill),
            assertz(corpse(Scene, Target)),
            set_kill_cooldown(3),
            format("你击杀了 ~w！~n", [Target])
          ; format("未选择有效目标，放弃击杀。~n")
        )
      )
    ).

set_kill_cooldown(N) :-
    retractall(kill_cooldown(_)),
    assertz(kill_cooldown(N)).

%% End of round updates
end_round_updates :-
    tick_cooldowns,
    update_tasks,
    update_suspicion_counters.



tick_cooldowns :-
    ( kill_cooldown(CD) -> CD1 is max(0, CD - 1), set_kill_cooldown(CD1) ; true ),
    ( scout_cooldown(CD2) -> CD21 is max(0, CD2 - 1), set_scout_cooldown(CD21) ; true ).

update_tasks :-
    forall(task_state(Id, Status, Rem, Occupant),
        ( Status = occupied ->
            ( alive(Occupant) ->
                Rem1 is Rem - 1,
                retract(task_state(Id, Status, Rem, Occupant)),
                ( Rem1 =< 0 ->
                    assertz(task_state(Id, completed, 0, none)),
                    total_tasks_remaining(T),
                    T1 is T - 1,
                    retract(total_tasks_remaining(T)),
                    assertz(total_tasks_remaining(T1)),
                    format("任务 ~w 完成！剩余任务 ~d。~n", [Id, T1])
                ; assertz(task_state(Id, occupied, Rem1, Occupant)),
                  format("任务 ~w 进度推进，剩余 ~d 轮。~n", [Id, Rem1])
                )
            ; retract(task_state(Id, Status, Rem, Occupant)),
              assertz(task_state(Id, free, Rem, none)),
              format("任务 ~w 占用者死亡，任务释放。~n", [Id])
            )
          ; true
        )
    ).

update_suspicion_counters :-
    findall(Name, alive(Name), AliveList),
    forall(member(Name, AliveList), update_character_counters(Name)).

update_character_counters(Name) :-
    location(Name, Scene),
    ( task_scene(Scene) ->
        set_not_in_task_counter(Name, 0)
    ; increment_not_in_task(Name)
    ),
    count_room_occupants(Scene, Count),
    ( Count =:= 1 -> increment_alone(Name) ; set_alone_counter(Name, 0) ).

set_not_in_task_counter(Name, Value) :-
    retractall(not_in_task_counter(Name, _)),
    assertz(not_in_task_counter(Name, Value)).

increment_not_in_task(Name) :-
    ( not_in_task_counter(Name, N) -> N1 is N + 1 ; N1 = 1 ),
    set_not_in_task_counter(Name, N1).

set_alone_counter(Name, Value) :-
    retractall(alone_counter(Name, _)),
    assertz(alone_counter(Name, Value)).

increment_alone(Name) :-
    ( alone_counter(Name, N) -> N1 is N + 1 ; N1 = 1 ),
    set_alone_counter(Name, N1).

count_room_occupants(Scene, Count) :-
    findall(Name, (alive(Name), location(Name, Scene)), List),
    length(List, Count).

%% Meeting logic
maybe_meeting :-
    ( meeting_needed(Reason) ->
        format("\n会议触发：~w~n", [Reason]),
        conduct_meeting(Reason),
        round_number(R),
        retract(last_meeting_round(_)),
        assertz(last_meeting_round(R))
    ; true).

meeting_needed(corpse) :- corpse(_, _), !.
meeting_needed(timer) :-
    round_number(R), last_meeting_round(L), R - L >= 3.

conduct_meeting(Reason) :-
    gather_votes(Votes),
    resolve_votes(Votes),
    ( Reason = corpse -> clear_one_corpse ; true ),
    show_status.

clear_one_corpse :-
    ( corpse(Scene, Victim) ->
        retract(corpse(Scene, Victim)),
        format("会议结束，清理尸体 ~w 于 ~w。~n", [Victim, Scene])
    ; true ).



gather_votes(VotePairs) :-
    findall(Voter-Vote, ai_vote(Voter, Vote), AIVotes),
    player_vote(PlayerVote),
    append(AIVotes, [fox-PlayerVote], VotePairs).

ai_vote(Voter, Vote) :-
    alive(Voter), Voter \= fox,
    ( known_fox(Fox) -> Vote = Fox
    ; suspicious_candidate(Voter, Vote) -> true
    ; random_other(Voter, Vote)
    ),
    format("~w 投票给 ~w。~n", [Voter, Vote]).

suspicious_candidate(Voter, Candidate) :-
    findall(Name, (alive(Name), Name \= Voter, is_suspicious(Name)), Candidates),
    Candidates \= [],
    random_member(Candidate, Candidates).

is_suspicious(Name) :-
    not_in_task_counter(Name, N1), N1 >= 3;
    alone_counter(Name, N2), N2 >= 2.

random_other(Self, Target) :-
    findall(Name, (alive(Name), Name \= Self), Others),
    random_member(Target, Others).

player_vote(Vote) :-
    format("玩家投票，输入角色名称（可见存活者列表查看）：~n"),
    findall(Name, alive(Name), Alive),
    format("存活角色: ~w~n", [Alive]),
    read_line_to_string(user_input, VoteStr),
    atom_string(VoteAtom, VoteStr),
    ( member(VoteAtom, Alive) -> Vote = VoteAtom
    ; format("输入无效，默认弃权，随机投票。~n"), random_other(fox, Vote)
    ).

resolve_votes(VotePairs) :-
    tally_votes(VotePairs, Counts),
    max_member(_-Max, Counts),
    findall(Name, member(Name-Max, Counts), Top),
    random_member(Eliminated, Top),
    eliminate(Eliminated, vote),
    format("投票结果：~w 被淘汰。~n", [Eliminated]).

tally_votes(VotePairs, Counts) :-
    findall(Candidate, member(_-Candidate, VotePairs), Candidates),
    sort(Candidates, Unique),
    findall(Name-Count, (member(Name, Unique), count_occurrences(Name, Candidates, Count)), Counts).

count_occurrences(X, List, Count) :- include(=(X), List, Filtered), length(Filtered, Count).

eliminate(Name, Reason) :-
    ( alive(Name) ->
        retract(alive(Name)),
        assertz(dead(Name)),
        format("~w 死亡，原因：~w。~n", [Name, Reason])
    ; true).


%% Movement helpers
move_character(Name, Scene) :-
    retract(location(Name, _)),
    assertz(location(Name, Scene)).

nearest_scene(Current, Targets, Scene, Distance) :-
    findall(D-S, (member(_-S, Targets), shortest_distance(Current, S, D)), Pairs),
    keysort(Pairs, [Distance-Scene|_]).

shortest_distance(Start, Goal, Distance) :-
    bfs([[Start]], Goal, Path),
    length(Path, Len),
    Distance is Len - 1.

bfs([[Goal|Rest]|_], Goal, [Goal|Rest]).
bfs([Path|Paths], Goal, Result) :-
    Path = [Current|_],
    findall([Next|Path], (connected(Current, Next), \+ member(Next, Path)), NewPaths),
    append(Paths, NewPaths, Queue),
    bfs(Queue, Goal, Result).

step_towards(Current, Goal, Next) :-
    bfs([[Current]], Goal, Path),
    reverse(Path, [Current,Next|_]).

%% Utilities





victory(rabbits) :- total_tasks_remaining(T), T =< 0.
victory(rabbits) :- \+ alive(fox).
victory(fox) :- findall(R, (alive(R), role(R, Type), Type \= fox), Rabbits), length(Rabbits, N), N =< 1.

show_status :-
    total_tasks_remaining(T),
    round_number(R),
    format("当前轮数：~d，剩余任务：~d。~n", [R, T]),
    findall(Id-Status-Rem-Occ, task_state(Id, Status, Rem, Occ), Tasks),
    forall(member(Id-St-Rem-Occ, Tasks), format("任务 ~w 状态：~w，剩余 ~d，占用者 ~w。~n", [Id, St, Rem, Occ])).


%% Helper predicates

task_in_scene(Scene, Id, Status, Occupant) :-
    base_task(Id, _, Scene, _),
    task_state(Id, Status, _, Occupant),
    Status \= completed.

task_scene(Scene) :- base_task(_, _, Scene, _).

