% This system may only distributed using the GNU General Public License
% because the following components contain GPL-ed code:
% 
%     /opt/local/lib/swipl-6.3.15/library/mime.pl
%     GNU Readline library
% 
% See http://www.swi-prolog.org/license.html for details on
% SWI-Prolog licensing policies supporting both free and non-free
% Software.

:- use_module(prolog_server).
:- use_module(consult).
:- use_module(storage).
:- use_module(lib/delegate).
:- use_module(lib/fetch).
:- use_module(lib/bags).
:- use_module(lib/admin/admin).
:- use_module(lib/admin/server_statistics).
:- use_module(lib/admin/applications).
:- use_module(lib/admin/change_passwd).

:- server(5003).
