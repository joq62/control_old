-module(db_service_def).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(service_def,
	{
	  id,
	  vsn,
	  source
	}).




-define(TABLE,service_def).
-define(RECORD,service_def).

start() ->
  %  mnesia:create_schema([node()]), %Should be started by db_mnesia
  %  mnesia:start(),
%    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}]),
 %   mnesia:wait_for_tables(?TABLE, 20000).   %Should be started by db_mnesia
    ok.
create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				{type,bag}]),
    mnesia:wait_for_tables(?TABLE, 20000).

create(Record) ->
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{ServiceId,Vsn,Source}||{?RECORD,ServiceId,Vsn,Source}<-Z].



read(ServiceId) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==ServiceId])),
    [{ServiceId,Vsn,Source}||{?RECORD,ServiceId,Vsn,Source}<-Z].

read(ServiceId,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==ServiceId,
		     X#?RECORD.vsn==Vsn])),
    [{ServiceId,Vsn,Source}||{?RECORD,ServiceId,Vsn,Source}<-Z].

update(Id,Vsn,NewVsn,NewSource) ->
    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.id==Id,X#?RECORD.vsn==Vsn],
		case ServiceDef of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{id=Id,vsn=NewVsn,source=NewSource})
		end
	end,
    mnesia:transaction(F).

delete(Id,Vsn) ->

    F = fun() -> 
		ServiceDef=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.id==Id,X#?RECORD.vsn==Vsn],
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
