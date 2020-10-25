-module(db_service_def).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(service_def,
	{
	  service_id,
	  vsn,
	  git_user_id
	}).




-define(TABLE,service_def).
-define(RECORD,service_def).

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables([?TABLE], 20000).
create_table(NodeList)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {disc_copies,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

create(ServiceId,Vsn,GitUserId)->
    Record=#?RECORD{ service_id=ServiceId,
		     vsn=Vsn,
		     git_user_id=GitUserId},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ServiceId,Vsn,Source}||{?RECORD,ServiceId,Vsn,Source}<-Z].



read(ServiceId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.service_id==ServiceId])),
    [{XServiceId,XVsn,XGitUserId}||{?RECORD,XServiceId,XVsn,XGitUserId}<-Z].

read(ServiceId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.service_id==ServiceId,
		     X#?RECORD.vsn==Vsn])),
    [{XServiceId,XVsn,XGitUserId}||{?RECORD,XServiceId,XVsn,XGitUserId}<-Z].

update(Id,Vsn,NewVsn,NewSource) ->
    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.service_id==Id,X#?RECORD.vsn==Vsn],
		case ServiceDef of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{service_id=Id,vsn=NewVsn,git_user_id=NewSource})
		end
	end,
    mnesia:transaction(F).

delete(Id,Vsn) ->

    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.service_id==Id,X#?RECORD.vsn==Vsn],
		case ServiceDef of
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
