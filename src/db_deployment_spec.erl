-module(db_deployment_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-record(deployment_spec,
	{
	  id,
	  vsn,
	  services
	}).

-define(TABLE,deployment_spec).
-define(RECORD,deployment_spec).

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

create(Id,Vsn,ServiceList) ->
    Record=#deployment_spec{id=Id,vsn=Vsn,services=ServiceList},
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{Id,Vsn,Services}||{?RECORD,Id,Vsn,Services}<-Z].



read(Id) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Id])),
    [{Id,Vsn,Services}||{?RECORD,Id,Vsn,Services}<-Z].

read(Id,Vsn) ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),
		   X#?RECORD.id==Id,
		     X#?RECORD.vsn==Vsn])),
    [{Id,Vsn,Services}||{?RECORD,Id,Vsn,Services}<-Z].


update(Id,Vsn,NewVsn,NewServices) ->
    F = fun() -> 
		DeploymentSpec=[X||X<-mnesia:read({?TABLE,Id}),
				   X#?RECORD.id==Id,X#?RECORD.vsn==Vsn],
		case DeploymentSpec of
		    []->
			mnesia:abort(?TABLE);
		    [S1]->
			mnesia:delete_object(S1), 
			mnesia:write(#?RECORD{id=Id,vsn=NewVsn,services=NewServices})
		end
	end,
    mnesia:transaction(F).

delete(Id,Vsn) ->

    F = fun() -> 
		DeploymentSpec=[X||X<-mnesia:read({?TABLE,Id}),
			    X#?RECORD.id==Id,X#?RECORD.vsn==Vsn],
		case DeploymentSpec of
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
