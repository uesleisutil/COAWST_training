function status = nc_write(ncfile, Vname, f, varargin)

%
% NC_WRITE:  Writes a variable into a NetCDF file
%
% status = nc_write(ncfile, Vname, f, Tindex)
%
% This routine writes in a generic variable into a NetCDF file.
%
% On Input:
%
%    ncfile      NetCDF file name (string)
%
%    Vname       NetCDF variable name to read (string)
%
%    f           Field (scalar, matrix or array)
%
%    Tindex      Optional, time index to write (integer):
%
%                  *  If tindex is not provided as an argument during
%                     function call, it is assumed that the entire
%                     variable is to be written. 
%
%                  *  If variable has an unlimited record dimension,
%                     tindex can be used to increase that dimension or
%                     replace an already existing record.
%
%                  *  If variable has the word "time" in its dimension
%                     name, tindex can be used to write at the specified
%                     the time record.
%
% On Output:
%
%    status      Error flag
%

<<<<<<< HEAD
% svn $Id: nc_write.m 895 2018-02-11 23:15:37Z arango $
%=========================================================================%
%  Copyright (c) 2002-2018 The ROMS/TOMS Group                            %
=======
% svn $Id: nc_write.m 996 2020-01-10 04:28:56Z arango $
%=========================================================================%
%  Copyright (c) 2002-2020 The ROMS/TOMS Group                            %
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
%    Licensed under a MIT/X style license                                 %
%    See License_ROMS.txt                           Hernan G. Arango      %
%=========================================================================%

% Initialize.

Tindex = [];
switch numel(varargin)
  case 1
    Tindex = varargin{1};
end
  
% Inquire about the contents of the NetCDF file.

Info = nc_inq(ncfile);

if (~any(strcmp({Info.Variables.Name}, Vname)))
  nc_inq(ncfile, true);
  disp(' ');
  error(['NC_WRITE: cannot find NetCDF variable: ',Vname]);
end

% Choose NetCDF file interface.

[method,~,~] = nc_interface(ncfile);

<<<<<<< HEAD
switch(method),
=======
switch(method)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  case {'native'}
    status = nc_write_matlab(ncfile, Vname, f, Tindex, Info);
  case {'mexnc'}
    status = nc_write_mexnc (ncfile, Vname, f, Tindex, Info);
  case {'java'}
    error('NC_WRITE: it is not possible to write into an OpenDAP file.');
  otherwise
    error('NC_WRITE: unable to determine NetCDF processing interface.');
end

return

%--------------------------------------------------------------------------

function status = nc_write_matlab(ncfile, Vname, f, Tindex, Info)

%
% NC_WRITE_MATLAB:  Writes a variable into a NetCDF file
%
% status = nc_write(ncfile, Vname, f, Tindex, Info)
%
% This routine writes the specified variable into a NetCDF file using the
% native interface.
%
% On Input:
%
%    ncfile      NetCDF file name (string)
%
%    Vname       NetCDF variable name to read (string)
%
%    f           Field (scalar or array)
%
%    Tindex      Optional, time index to write:
%
%                  *  If Tindex is not provided as an argument during
%                     function call, it is assumed that entire variable
%                     is to be written. 
%
%                  *  If variable has an unlimited record dimension,
%                     Tindex can be used to increase that dimension or
%                     replace an already existing record.
%
%                  *  If variable has the word "time" in its dimension
%                     name, Tindex can be use to write at the specified
%                     the time record.
%
%    Info        NetCDF information structure (struct array)
%
% On Output:
%
%    status      Error flag
%

% Initialize.

if (isempty(Tindex))
  time_rec = false;
else
  time_rec = true;
end

got.FillValue     = false;
got.missing_value = false;
got.RecDim        = false;

ivar   = find(strcmp({Info.Variables.Name}, Vname));
nvdims = length(Info.Variables(ivar).Dimensions);
nvatts = length(Info.Variables(ivar).Attributes);
nctype = Info.Variables(ivar).ncType;

status = 0;

% Check if there is an unlimited dimension or a time dimension.

index = 0;
<<<<<<< HEAD
if (nvdims > 0),
  for n=1:nvdims,
    dname = char(Info.Variables(ivar).Dimensions(n).Name);
    if (Info.Variables(ivar).Dimensions(n).Unlimited ||                 ...
        ~isempty(strfind(dname,'time'))),
=======
if (nvdims > 0)
  for n=1:nvdims
    dname = char(Info.Variables(ivar).Dimensions(n).Name);
    if (Info.Variables(ivar).Dimensions(n).Unlimited ||                 ...
        ~isempty(strfind(dname,'time')))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      got.RecDim = true;
      index = n;
    end
  end
end

% Inquire information about the attributes.

<<<<<<< HEAD
if (nvatts > 0),
  for n=1:nvatts,
=======
if (nvatts > 0)
  for n=1:nvatts
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    aname = char(Info.Variables(ivar).Attributes(n).Name);
    switch aname
      case {'_FillValue', '_fillvalue', 'missing_value'}
        spval = Info.Variables(ivar).Attributes(n).Value;
<<<<<<< HEAD
        if (strcmp(aname, 'missing_value')),
=======
        if (strcmp(aname, 'missing_value'))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
          got.missing_value = true;
        else
          got.FillValue = true;
        end
    end
  end
end

% Set start and count indices to process.

<<<<<<< HEAD
if (nvdims > 0),
  start = zeros([1 nvdims]);
  count = Inf([1 nvdims]); 

  for n=1:nvdims,
=======
if (nvdims > 0)
  start = zeros([1 nvdims]);
  count = Inf([1 nvdims]); 

  for n=1:nvdims
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    start(n) = 0;
    count(n) = Info.Variables(ivar).Dimensions(n).Length;
  end
end

%  It writing specific time record, reset variable bounds.  Check
%  for files having more than one datum in the unlimited dimension
% (like variational data assimilation files).

datum = false;
<<<<<<< HEAD
if (got.RecDim  && (nvdims == 1)),
  if (length(f) > 1)
    datum = true;
    if (time_rec),
=======
if (got.RecDim  && (nvdims == 1))
  if (length(f) > 1)
    datum = true;
    if (time_rec)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      start(index) = Tindex-1;
    else
      start(index) = 0;
    end
    count(index) = length(f);
  end
end

<<<<<<< HEAD
if (~datum && time_rec && (index > 0)),
=======
if (~datum && time_rec && (index > 0))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  start(index) = Tindex-1;
  count(index) = 1;
end

%--------------------------------------------------------------------------
% If _FillValue attribute, replace NaNs with fill value.
%--------------------------------------------------------------------------

% Compute the minimum and maximum of the data to write.

fmin = min(f(:));
fmax = max(f(:));

% Replace NaNs if any with fill value.

<<<<<<< HEAD
if (got.FillValue),
=======
if (got.FillValue)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  ind = isnan(f);
  if (~isempty(ind))
    switch nctype
      case (netcdf.getConstant('nc_double'))
        f(ind) = double(spval);
      case (netcdf.getConstant('nc_float'))
        f(ind) = single(spval);
      case (netcdf.getConstant('nc_int'))
        f(ind) = int32(spval);
      case (netcdf.getConstant('nc_short'))
        f(ind) = int16(spval);
      case (netcdf.getConstant('nc_byte'))
        f(ind) = int8(spval);
      otherwise
        f(ind) = spval;
    end
  end
end

%--------------------------------------------------------------------------
% Write out variable into NetCDF file.
%--------------------------------------------------------------------------

% Open NetCDF file and get variable ID.

ncid  = netcdf.open(ncfile, 'nc_write');
varid = netcdf.inqVarID(ncid, Vname);

% Write out data.

<<<<<<< HEAD
if (nvdims > 0),
=======
if (nvdims > 0)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  netcdf.putVar(ncid, varid, start, count, f)
else
  netcdf.putVar(ncid, varid, f)
end  

% Close NetCDF file.

netcdf.close(ncid);

% Report.

<<<<<<< HEAD
if (nvdims > 1),
  text(1:19)=' ';
  text(1:length(Vname))=Vname;
  if (nargin > 3),
=======
if (nvdims > 1)
  text(1:19)=' ';
  text(1:length(Vname))=Vname;
  if (got.RecDim)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    disp(['Wrote ',sprintf('%19s',text),                                ...
          ' into record: ',num2str(Tindex,'%4.4i'),                     ...
          ', Min=',sprintf('%12.5e',fmin),                              ...
          ' Max=',sprintf('%12.5e',fmax)]);
  else
    disp(['Wrote ',sprintf('%19s',text),                                ...
          ' Min=',sprintf('%12.5e',fmin),                               ...
          ' Max=',sprintf('%12.5e',fmax)]);
  end
end

return

%--------------------------------------------------------------------------

function status = nc_write_mexnc(ncfile, Vname, f, Tindex, Info)

%
% NC_WRITE_MEXNC:  Writes a variable into a NetCDF file
%
% status = nc_write(ncfile, Vname, f, Tindex, Info)
%
% This routine writes the specified variable into a NetCDF file using the
% MEXNC interface.
%
% On Input:
%
%    ncfile      NetCDF file name (character string)
%
%    Vname       NetCDF variable name to read (character string)
%
%    f           Field (scalar, matrix or array)
%
%    Tindex      Optional, time index to write (integer):
%
%                  *  If Tindex is not provided as an argument during
%                     function call, it is assumed that entire variable
%                     is to be written. 
%
%                  *  If variable has an unlimited record dimension,
%                     Tindex can be used to increase that dimension or
%                     replace an already existing record.
%
%                  *  If variable has the word "time" in its dimension
%                     name, Tindex can be use to write at the specified
%                     the time record.
%
% On Output:
%
%    status      Error flag
%

% Initialize.
  
if (isempty(Tindex))
  time_rec = false;
else
  time_rec = true;
end

% Open NetCDF file.

[ncid]=mexnc('ncopen',ncfile,'NC_WRITE');
<<<<<<< HEAD
if (ncid == -1),
=======
if (ncid == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  error(['NC_WRITE_MEXNC: ncopen - unable to open file: ', ncfile]);
end

% Supress all error messages from NetCDF.

mexnc('setopts',0);

%--------------------------------------------------------------------------
% Inquire about requested variable.
%--------------------------------------------------------------------------

% Get variable ID.

[varid]=mexnc('ncvarid',ncid,Vname);
<<<<<<< HEAD
if (varid < 0),
=======
if (varid < 0)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  [status]=mexnc('ncclose',ncid);
  nc_inq(ncfile);
  disp('  ');
  error(['NC_WRITE_MEXNC: ncvarid - cannot find variable: ',Vname])
end

% Inquire about unlimmited dimension.

[~,~,~,recdim,status]=mexnc('ncinquire',ncid);
<<<<<<< HEAD
if (status == -1),
=======
if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  error(['NC_WRITE_MEXNC: ncinquire - cannot inquire file: ',ncfile])
end

% Get information about requested variable.

[Vname,nctype,nvdims,dimids,nvatts,status]=mexnc('ncvarinq',ncid,varid);
<<<<<<< HEAD
if (status == -1),
=======
if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  error(['NC_WRITE_MEXNC: ncvarinq - unable to inquire about ',         ...
         'variable: ',Vname]);
end

% Inquire about dimensions.

index = 0;
unlimited = false;

<<<<<<< HEAD
if (nvdims > 0),
  start = zeros([1 nvdims]);
  count = Inf([1 nvdims]); 

  for n=1:nvdims,
    [dname,size,status]=mexnc('ncdiminq',ncid,dimids(n));
    if (status == -1),
=======
if (nvdims > 0)
  start = zeros([1 nvdims]);
  count = Inf([1 nvdims]); 

  for n=1:nvdims
    [dname,size,status]=mexnc('ncdiminq',ncid,dimids(n));
    if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      error(['NC_WRITE_MEXNC: ncdiminq - unable to inquire about ',     ...
             'dimension ID: ',num2str(dimids(n))])
    else
      start(n) = 0;
      count(n) = size;
<<<<<<< HEAD
      if ((dimids(n) == recdim) || ~isempty(strfind(dname,'time'))),
=======
      if ((dimids(n) == recdim) || ~isempty(strfind(dname,'time')))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
        unlimited = true;
        index = n;
      end
    end
  end
end

% Inquire about the _FillValue attribute.

got.FillValue = false;

<<<<<<< HEAD
for i = 0:nvatts-1,
=======
for i = 0:nvatts-1
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  [attnam,status]=mexnc('inq_attname',ncid,varid,i);
  if (status == -1)
    error(['NC_WRITE_MEXNC: inq_attname: error while inquiring ',       ...
           'attribute ', num2str(i)]);
  end
  lstr=length(attnam);
  [atype,status]=mexnc('inq_atttype',ncid,varid,attnam(1:lstr));
  if (status == -1)
    error(['NC_WRITE_MEXNC: inq_atttype: error while inquiring ',       ...
           'attribute ', num2str(i)]);
<<<<<<< HEAD
  end,
  if (strcmp(attnam(1:lstr),'_FillValue')     ||                        ...
      strcmp(attnam(1:lstr),'missing_value')),
=======
  end
  if (strcmp(attnam(1:lstr),'_FillValue')     ||                        ...
      strcmp(attnam(1:lstr),'missing_value'))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    switch atype
      case (nc_constant('nc_double'))
        [spval,status] = mexnc('get_att_double',ncid,varid,attnam(1:lstr));
        myfunc = 'get_att_double';
      case (nc_constant('nc_float'))
        [spval,status] = mexnc('get_att_float' ,ncid,varid,attnam(1:lstr));
        myfunc = 'get_att_float';
      case (nc_constant('nc_int'))
        [spval,status] = mexnc('get_att_int'   ,ncid,varid,attnam(1:lstr));
        myfunc = 'get_att_int';
      case (nc_constant('nc_short'))
        [spval,status] = mexnc('get_att_short' ,ncid,varid,attnam(1:lstr));
        myfunc = 'get_att_short';
      case (nc_constant('nc_byte'))
        [spval,status] = mexnc('get_att_schar' ,ncid,varid,attnam(1:lstr));
        myfunc = 'get_att_schar';
      otherwise
        [spval,status] = mexnc('ncattget'      ,ncid,varid,attnam(1:lstr));
        myfunc = 'ncattget';
    end
<<<<<<< HEAD
    if (status == -1),
=======
    if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      error(['NC_WRITE_MEXNC: ',myfunc,' - error while reading ',       ...
             attnam(1:lstr),' attribute'])
    end
    got.FillValue = true;
  end
end

% It writing specific time record, reset variable bounds.  Check
% for for files having the more than one datum in the unlimited
% dimension (like variational data assimilation files).

datum = false;
<<<<<<< HEAD
if (unlimited && (nvdims == 1)),
  if (length(f) > 1)
    datum = true;
    if (time_rec),
=======
if (unlimited && (nvdims == 1))
  if (length(f) > 1)
    datum = true;
    if (time_rec)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
      start(index) = Tindex-1;
    else
      start(index) = 0;
    end
    count(index) = length(f);
  end
end

<<<<<<< HEAD
if (~datum && time_rec && (index > 0)),
=======
if (~datum && time_rec && (index > 0))
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  start(index) = Tindex-1;
  count(index) = 1;
end

%--------------------------------------------------------------------------
% If '_FillValue' attribute, replace NaNs with fill value.
%--------------------------------------------------------------------------

% Compute the minimum and maximum of the data to write.

fmin = min(f(:));
fmax = max(f(:));

% Replace NaNs if any with fill value.

<<<<<<< HEAD
if (got.FillValue),
=======
if (got.FillValue)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  ind = isnan(f);
  if (~isempty(ind))
    switch nctype
      case (nc_constant('nc_double'))
        f(ind) = double(spval);
      case (nc_constant('nc_float'))
        f(ind) = single(spval);
      case (nc_constant('nc_int'))
        f(ind) = int32(spval);
      case (nc_constant('nc_short'))
        f(ind) = int16(spval);
      case (nc_constant('nc_byte'))
        f(ind) = int8(spval);
      otherwise
        f(ind) = spval;
    end
  end
end

%--------------------------------------------------------------------------
% Write out variable into NetCDF file.
%--------------------------------------------------------------------------

<<<<<<< HEAD
if (nvdims > 0),
=======
if (nvdims > 0)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  switch nctype
    case (nc_constant('nc_double'))
      status = mexnc('put_vara_double',ncid,varid,start,count,f);
      myfunc = 'put_vara_double';
    case (nc_constant('nc_float'))
      status = mexnc('put_vara_float' ,ncid,varid,start,count,f);
      myfunc = 'put_vara_float';
    case (nc_constant('nc_int'))
      status = mexnc('put_vara_int'   ,ncid,varid,start,count,f);
      myfunc = 'put_vara_int';
    case (nc_constant('nc_short'))
      status = mexnc('put_vara_short' ,ncid,varid,start,count,f);
      myfunc = 'put_vara_short';
    case (nc_constant('nc_byte'))
      status = mexnc('put_vara_schar' ,ncid,varid,start,count,f);
      myfunc = 'put_vara_schar';
    case (nc_constant('nc_char'))
      status = mexnc('put_vara_text'  ,ncid,varid,start,count,f);
      myfunc = 'put_var_text';
    otherwise
      status = mexnc('ncvarput'       ,ncid,varid,start,count,f);
  end
else
  switch nctype
    case (nc_constant('nc_double'))
      status = mexnc('put_var_double',ncid,varid,f);
      myfunc = 'put_var_double';
    case (nc_constant('nc_float'))
      status = mexnc('put_var_float' ,ncid,varid,f);
      myfunc = 'put_var_float';
    case (nc_constant('nc_int'))
      status = mexnc('put_var_int'   ,ncid,varid,f);
      myfunc = 'put_var_int';
    case (nc_constant('nc_short'))
      status = mexnc('put_var_short' ,ncid,varid,f);
      myfunc = 'put_var_short'; 
    case (nc_constant('nc_byte'))
      status = mexnc('put_var_schar' ,ncid,varid,f);
      myfunc = 'put_var_schar';
    case (nc_constant('nc_char'))
      status = mexnc('put_var_text'  ,ncid,varid,f);
      myfunc = 'put_var_text';
   otherwise
      status = mexnc('ncvarput1'     ,ncid,varid,f);
      myfunc = 'ncvarput1';
  end
end

% Report.

<<<<<<< HEAD
if (status ~= -1 && nvdims > 1),
  text(1:19)=' ';
  text(1:length(Vname))=Vname;
  if (nargin > 3),
=======
if (status ~= -1 && nvdims > 1)
  text(1:19)=' ';
  text(1:length(Vname))=Vname;
  if (got.RecDim)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
    disp(['Wrote ',sprintf('%19s',text),                                ...
          ' into record: ',num2str(Tindex,'%4.4i'),                     ...
          ', Min=',sprintf('%12.5e',fmin),                              ...
          ' Max=',sprintf('%12.5e',fmax)]);
  else
    disp(['Wrote ',sprintf('%19s',text),                                ...
          ' Min=',sprintf('%12.5e',fmin),                               ...
          ' Max=',sprintf('%12.5e',fmax)]);
  end
end

<<<<<<< HEAD
if (status == -1),
=======
if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  error(['NC_WRITE_MEXNC: ',myfunc,' - error while writting ',          ...
         'variable: ', Vname, sprintf('\n'), blanks(16),                ...
          mexnc('strerror',status)]);
end

%--------------------------------------------------------------------------
% Close NetCDF file.
%--------------------------------------------------------------------------

status = mexnc('ncclose',ncid);
<<<<<<< HEAD
if (status == -1),
=======
if (status == -1)
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
  error(['NC_WRITE_MEXNC: ncclose - unable to close NetCDF file: ',     ...
        ncfile]);
end

return
