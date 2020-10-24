%%% -------------------------------------------------------------------
%%% @author : joqerlang
%%% @doc : ets dbase for master service to manage app info , catalog  
%%%
%%% -------------------------------------------------------------------
-module(deployment).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%-compile(export_all).
-export([create_spec/3,
	 read_spec/2,
	 delete_spec/2]).

%% --------------------------------------------------------------------
%% Function:create(ServiceId,Vsn,HostId,VmId)
%% Description: Starts vm and deploys services 
%% Returns: ok |{error,Err}
%
%% --------------------------------------------------------------------
create_spec(AppId,AppVsn,ServiceList)->
    Reply=case db_deployment_spec:read(AppId,AppVsn) of
	      []->
		  db_deployment_spec:create(AppId,AppVsn,ServiceList),
		  ok;
	      Err->
		  {error,[already_defined,AppId,AppVsn]}
	  end,
    Reply.

read_spec(AppId,AppVsn)->
    db_deployment_spec:read(AppId,AppVsn).

delete_spec(AppId,AppVsn)->
    db_deployment_spec:delete(AppId,AppVsn).

%% ====================================================================
%% External functions
%% ====================================================================


%% --------------------------------------------------------------------
%% 
%%
%% --------------------------------------------------------------------
