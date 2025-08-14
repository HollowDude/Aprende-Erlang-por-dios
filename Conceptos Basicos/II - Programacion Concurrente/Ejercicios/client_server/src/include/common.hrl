-define(Server_Reg_Name, 'server').
-define(Client_Reg_Name, 'client_of_serv').
-define(Server_Node, 'c_serv@Hollow').

-define(Msg_login(From, Name), {From, login, Name}).
-define(Msg_logout(From), {From, logout}).
-define(Msg_listar(From), {From, listar}).

-define(Rsp_Listar(List), {listar, List}).
-define(Server_Stop(XQ), {serv, stop, XQ}).
-define(Server_Login, {serv, logged_in}).