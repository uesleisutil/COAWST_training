function [Uscope,Vscope]=uv_scope(Rscope);

%
% UV_SCOPE:  Computes adjoint sensitivity scope U- and V-mask
%
%  [Uscope,Vscope]=uv_scope(Rscope)
%
%  This function computes the adjoint sensitivity scope masks on
%  U- and V-points from the scope on RHO-points.
%
%  On Input:
%
%    Rscope        Scope mask on RHO-points (real matrix).
%
%  On Output:
%
%    Uscope        Scope mask on U-points (real matrix).
%    Vscope        Scope mask on V-points (real matrix).
%

<<<<<<< HEAD
% svn $Id: uv_scope.m 895 2018-02-11 23:15:37Z arango $
%===========================================================================%
%  Copyright (c) 2002-2018 The ROMS/TOMS Group                              %
=======
% svn $Id: uv_scope.m 996 2020-01-10 04:28:56Z arango $
%===========================================================================%
%  Copyright (c) 2002-2020 The ROMS/TOMS Group                              %
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
%    Licensed under a MIT/X style license                                   %
%    See License_ROMS.txt                           Hernan G. Arango        %
%===========================================================================%

[Lp,Mp]=size(Rscope);
L=Lp-1;
M=Mp-1;

%  Scope mask on U-points.

Uscope(1:L,1:Mp)=Rscope(2:Lp,1:Mp).*Rscope(1:L,1:Mp);

%  Scope mask on V-points.

Vscope(1:Lp,1:M)=Rscope(1:Lp,2:Mp).*Rscope(1:Lp,1:M);

return
