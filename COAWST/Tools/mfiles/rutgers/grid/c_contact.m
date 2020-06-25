function c_contact(ncname, spherical, Ngrids, Ndatum)

%
% C_CONTACT:  Creates ROMS nesting Contact Points NetCDF file
%
% c_contact(ncname, spherical, Ngrids, Ndatum)
%
% This function creates a new Contact Points NetCDF file for a ROMS
% nesting application. The Contact Points data is written elsewhere.
% If the Ndatum argument is not provided, the "datum" dimension is
% is "unlimited".
%
% On Input:
%
%    ncname      Ouptut Contact Points NetCDF file name (string)
%
%    spherical   Spherical grid switch
%
%    Ngrids      Number of nested grids
%
%    Ndatum      Total number of contact points (optional)
%

<<<<<<< HEAD
% svn $Id: c_contact.m 895 2018-02-11 23:15:37Z arango $
%=========================================================================%
%  Copyright (c) 2002-2018 The ROMS/TOMS Group                            %
=======
% svn $Id: c_contact.m 996 2020-01-10 04:28:56Z arango $
%=========================================================================%
%  Copyright (c) 2002-2020 The ROMS/TOMS Group                            %
>>>>>>> b1b191b5bc4e1e579b5a1fc399451b14a647f834
%    Licensed under a MIT/X style license                                 %
%    See License_ROMS.txt                           Hernan G. Arango      %
%=========================================================================%

% Set NetCDF file dimensions.
  
Ncontact  = (Ngrids-1)*2;
nLweights = 4;                      % Linear    interpolation weights
nQweights = 9;                      % Quadratic interpolation weights

if (nargin < 4),
  Ndatum = netcdf.getConstant('UNLIMITED');
end

% ROMS FillValue.

FillValue = 1.0d+37;

%--------------------------------------------------------------------------
% Create Contact Points NetCDF.
%--------------------------------------------------------------------------

% Create file.

mode = netcdf.getConstant('CLOBBER');
mode = bitor(mode,netcdf.getConstant('64BIT_OFFSET'));

ncid = netcdf.create(ncname,mode);

% Define dimensions.

Did.Ngrids    = netcdf.defDim(ncid, 'Ngrids'   ,Ngrids);
Did.Ncontact  = netcdf.defDim(ncid, 'Ncontact' ,Ncontact);
Did.nLweights = netcdf.defDim(ncid, 'nLweights',nLweights);
Did.nQweights = netcdf.defDim(ncid, 'nQweights',nQweights);
Did.datum     = netcdf.defDim(ncid, 'datum'    ,Ndatum);

% Define global attributes.

varid =  netcdf.getConstant('nc_global');
netcdf.putAtt(ncid, varid, 'type',                                      ...
              'ROMS Nesting Contact Regions Data');

%--------------------------------------------------------------------------
% Define variables.
%--------------------------------------------------------------------------

% Spherical switch.

varid = netcdf.defVar(ncid, 'spherical',                                ...
                      netcdf.getConstant('nc_int'), []);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'grid type logical switch') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'Cartesian spherical');

% Number of interior RHO-points.

varid = netcdf.defVar(ncid, 'Lm',                                       ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'number of interior RHO-points in the I-direction') ;

varid = netcdf.defVar(ncid, 'Mm',                                       ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'number of interior RHO-points in the J-direction') ;

% Information variables.

varid = netcdf.defVar(ncid, 'refine_factor',                            ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'refinement factor from donor grid') ;

varid = netcdf.defVar(ncid, 'coincident',                               ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'coincident donor and receiver grids logical switch') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'false true');

varid = netcdf.defVar(ncid, 'composite',                                ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'composite grid type logical switch') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'false true');

varid = netcdf.defVar(ncid, 'mosaic',                                   ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'mosaic grid type logical switch') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'false true');

varid = netcdf.defVar(ncid, 'refinement',                               ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'refinement grid type logical switch') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'false true');

% Contact Points vertical interpolation switch.

varid = netcdf.defVar(ncid, 'interpolate',                              ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'vertical interpolation at contact points logical switch');
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'false true');

% Contact region donor and receiver grid.

varid = netcdf.defVar(ncid, 'donor_grid',                               ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'data donor grid number');

varid = netcdf.defVar(ncid, 'receiver_grid',                            ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'data receiver grid number');

% Refinement grid extraction coordinates, if any.

varid = netcdf.defVar(ncid, 'I_left',                                   ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      ['donor grid I-left index at PSI points ',                ...
               'used to extract refinement grid']);
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              int32(-999));

varid = netcdf.defVar(ncid, 'I_right',                                  ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      ['donor grid I-right index at PSI points ',               ...
               'used to extract refinement grid']);
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              int32(-999));

varid = netcdf.defVar(ncid, 'J_bottom',                                 ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      ['donor grid J-bottom index at PSI points ',              ...
               'used to extract refinement grid']);
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              int32(-999));

varid = netcdf.defVar(ncid, 'J_top',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ngrids);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      ['donor grid J-top index at PSI points ',                 ...
               'used to extract refinement grid']);
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              int32(-999));

% Contact points start and end indices in data vector.

varid = netcdf.defVar(ncid, 'NstrR',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'starting contact RHO-point index in data vector');

varid = netcdf.defVar(ncid, 'NendR',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'ending contact RHO-point index in data vector');

varid = netcdf.defVar(ncid, 'NstrU',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'starting contact U-point index in data vector');

varid = netcdf.defVar(ncid, 'NendU',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'ending contact U-point index in data vector');

varid = netcdf.defVar(ncid, 'NstrV',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'starting contact V-point index in data vector');

varid = netcdf.defVar(ncid, 'NendV',                                    ...
                      netcdf.getConstant('nc_int'), Did.Ncontact);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'ending contact V-point index in data vector');

% Contact region for each contact point.

varid = netcdf.defVar(ncid, 'contact_region',                           ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'contact region number');

% Contact points on receiver grid physical boundary.

varid = netcdf.defVar(ncid, 'on_boundary',                              ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
	      'contact point on receiver grid physical boundary') ;
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [int32(0), int32(1), int32(2), int32(3), int32(4)]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'other western southern eastern northern');

% Donor grid cell indices.

varid = netcdf.defVar(ncid, 'Idg',                                      ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'I-left index of donor cell containing contact point');

varid = netcdf.defVar(ncid, 'Jdg',                                      ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'J-bottom index of donor cell containing contact point');

% Receiver grid indices.

varid = netcdf.defVar(ncid, 'Irg',                                      ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'receiver grid I-index of contact point');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');

varid = netcdf.defVar(ncid, 'Jrg',                                      ...
                      netcdf.getConstant('nc_int'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'receiver grid J-index of contact point');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');

% Donor grid cell bottom-left (XI,ETA) coordinates.

varid = netcdf.defVar(ncid, 'xi_dg',                                    ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              ['XI-coordinate of donor cell bottom-left corner ',       ...
               'containing contact point']);

varid = netcdf.defVar(ncid, 'eta_dg',                                   ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              ['ETA-coordinate of donor cell bottom-left corner ',      ...
               'containing contact point']);

% Receiver grid contact point (XI,ETA) coordinates.
  
varid = netcdf.defVar(ncid, 'xi_rg',                                    ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'XI-coordinate of receiver grid contact points');

varid = netcdf.defVar(ncid, 'eta_rg',                                   ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'ETA-location of receiver grid contact points');

% Donor grid cell bottom-left coordinates.

varid = netcdf.defVar(ncid, 'Xdg',                                      ...
                      netcdf.getConstant('nc_double'), Did.datum);
if (spherical),
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                ['longitude of donor cell bottom-left corner ',         ...
                 'containing contact point']);
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'degree_east');
  netcdf.putAtt(ncid, varid, 'standard_name',                           ...
                'longitude');
else
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                ['X-location of donor cell bottom-left corner ',        ...
                 'containing contact point']);
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'meter');
end

varid = netcdf.defVar(ncid, 'Ydg',                                      ...
                      netcdf.getConstant('nc_double'), Did.datum);
if (spherical),
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                ['latitude of donor cell bottom-left corner ',          ...
                 'containing contact point']);
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'degree_north');
  netcdf.putAtt(ncid, varid, 'standard_name',                           ...
                'latitude');
else
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                ['Y-location of donor cell bottom-left corner ',        ...
                 'containing contact point']);
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'meter');
end

% Receiver grid contact point coordinates.
  
varid = netcdf.defVar(ncid, 'Xrg',                                      ...
                      netcdf.getConstant('nc_double'), Did.datum);
if (spherical),
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                'longitude of receiver grid contact points');
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'degree_east');
  netcdf.putAtt(ncid, varid, 'standard_name',                           ...
                'longitude');
else
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                'X-location of receiver grid contact points');
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'meter');
end

varid = netcdf.defVar(ncid, 'Yrg',                                      ...
                      netcdf.getConstant('nc_double'), Did.datum);
if (spherical),
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                'latitude of receiver grid contact points');
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'degree_north');
  netcdf.putAtt(ncid, varid, 'standard_name',                           ...
                'latitude');
else
  netcdf.putAtt(ncid, varid, 'long_name',                               ...
                'Y-location of receiver grid contact points');
  netcdf.putAtt(ncid, varid, 'units',                                   ...
                'meter');
end

% Horizontal linear interpolation weights.

varid = netcdf.defVar(ncid, 'Lweight',                                  ...
                      netcdf.getConstant('nc_double'),                  ...
                      [Did.nLweights Did.datum]);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'horizontal linear interpolation weights');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');

% Horizontal quadratic interpolation weights.

varid = netcdf.defVar(ncid, 'Qweight',                                  ...
                      netcdf.getConstant('nc_double'),                  ...
                      [Did.nQweights Did.datum]);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'horizontal quadratic interpolation weights');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');

% Grid bathymetry.

varid = netcdf.defVar(ncid, 'h',                                        ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'bathymetry at RHO-points');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'meter');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

% Coriolis parameter.

varid = netcdf.defVar(ncid, 'f',                                        ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'Coriolis parameter at RHO-points');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'second-1');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

% Curvilinear metrics.

varid = netcdf.defVar(ncid, 'pm',                                       ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'curvilinear coordinate metric in XI');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'meter-1');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
             'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

varid = netcdf.defVar(ncid, 'pn',                                       ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'curvilinear coordinate metric in ETA');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'meter-1');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

varid = netcdf.defVar(ncid, 'dndx',                                     ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'XI-derivative of inverse metric factor pn');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'meter');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

varid = netcdf.defVar(ncid, 'dmde',                                     ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'ETA-derivative of inverse metric factor pm');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'meter');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

% Grid rotation angle.

varid = netcdf.defVar(ncid, 'angle',                                    ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'angle between XI-axis and EAST');
netcdf.putAtt(ncid, varid, 'units',                                     ...
              'radians');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');
netcdf.putAtt(ncid, varid, '_FillValue',                                ...
              FillValue);

% Land/Sea masking.

varid = netcdf.defVar(ncid, 'mask',                                     ...
                      netcdf.getConstant('nc_double'), Did.datum);
netcdf.putAtt(ncid, varid, 'long_name',                                 ...
              'land-sea mask of contact points');
netcdf.putAtt(ncid, varid, 'flag_values',                               ...
              [0 1]);
netcdf.putAtt(ncid, varid, 'flag_meanings',                             ...
              'land water');
netcdf.putAtt(ncid, varid, 'coordinates',                               ...
              'Xrg Yrg');

%--------------------------------------------------------------------------
% Close file.
%--------------------------------------------------------------------------

netcdf.endDef(ncid);

netcdf.close(ncid);

return
