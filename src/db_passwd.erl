-module(db_passwd).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(passwd,
	{
	  user_id,
	  passwd
	}).




-define(TABLE,passwd).
-define(RECORD,passwd).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables(?TABLE, 20000).

create(Id,PassWd) ->
    Record=#?RECORD{user_id=Id,passwd=PassWd},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{XId,XPwd}||{?RECORD,XId,XPwd}<-Z].



read(Id) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.user_id==Id])),
    [{XId,XPwd}||{?RECORD,XId,XPwd}<-Z].

read(Id,Pwd) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.user_id==Id,
		     X#?RECORD.passwd==Pwd])),
    [{XId,XPwd}||{?RECORD,XId,XPwd}<-Z].

update(Id,Pwd,NewPwd) ->
    F = fun() -> 
		PassWd=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.user_id==Id,X#?RECORD.passwd==Pwd],
		case PassWd of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{user_id=Id,passwd=NewPwd})
		end
	end,
    mnesia:transaction(F).

delete(Id,Pwd) ->

    F = fun() -> 
		PassWd=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.user_id==Id,X#?RECORD.passwd==Pwd],
		case PassWd of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1) 
		end
	end,
    mnesia:transaction(F).


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.

%%-------------------------------------------------------------------------