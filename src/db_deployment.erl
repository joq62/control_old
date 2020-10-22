-module(db_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").


-record(deployment,
	{
	  id,
	  vsn,
	  service_id,
	  service_vsn,
	  vm
	}).

-define(TABLE,deployment).
-define(RECORD,deployment).

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
    [{Id,Vsn,ServiceId,ServiceVsn,Vm}||{?RECORD,Id,Vsn,ServiceId,ServiceVsn,Vm}<-Z].



read(Id) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Id])),
    [{SpecId,Vsn,ServiceId,ServiceVsn,Vm}||{?RECORD,SpecId,Vsn,ServiceId,ServiceVsn,Vm}<-Z].


delete(Id,Vsn,ServiceId,ServiceVsn,Vm) ->

    F = fun() -> 
		Deployment=[X||X<-mnesia:read({?TABLE,Id}),
			       X#?RECORD.id==Id,
			       X#?RECORD.vsn==Vsn,
			       X#?RECORD.service_id==ServiceId,
			       X#?RECORD.service_vsn==ServiceVsn,
			       X#?RECORD.vm==Vm],
		case Deployment of
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
