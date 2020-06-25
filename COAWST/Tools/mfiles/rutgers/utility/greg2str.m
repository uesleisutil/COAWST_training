function [str]=greg2str(gtime)

%
% GREG2STR: Converts Gregorian date array to string
% 
% [str]=greg2str(gtime)
%
% This function Converts Gregorian date to string for display.
% It does not include seconds.
%
% On Input:
%
%    gtime       Gregorian date. A six component vector:
%                  gtime=[yyyy month day hour minute second]
%
% On Output:
%
%    str         Gregorian date string.
%                  greg2str([1968 5 23 0 0 0]) = '1968/05/23 00:00'

<<<<<<< HEAD
% svn $Id: greg2str.m 895 2018-02-11 23:15:37Z arango $
%===========================================================================%
%  Copyright (c) 2002-2018 The ROMS/TOMS Group                              %
=======
% svn $Id: greg2str.m 996 2020-01-10 04:28:56Z arango $
%===========================================================================%
%  Copyright (c) 2002-2020 The ROMS/TOMS Group                              %
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
%    Licensed under a MIT/X style license                                   %
%    See License_ROMS.txt                               Rich Signell        %
%===========================================================================%

str=sprintf('%4.4d/%2.2d/%2.2d %2.2d:%2.2d',gtime(1:5));

return
