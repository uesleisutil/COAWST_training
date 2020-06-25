#!/home/uesleisutil/anaconda3/bin/python python
# -*- coding: utf-8 -*-

"""
File name:      roms_wrf_horizontal.py
Author:         Ueslei Adriano Sutil
Email:          uesleisutil1@gmail.com
Created:        27 February 2019
Last modified:  24 June 2020
Version:        3.5.1
Python:         3.7.1

Create horizontal plots from ROMS (his) and WRF-ARW outputs, displaying:
    - Sea surface temperature (Contour; °C);
    - Heat fluxes (Contour; W m.s⁻²);
    - Wind vectors at 10 m (Vector; m.s⁻¹);
    - Ocean currents at surface (Vector; m.s⁻¹);

If you are using conda, do the following:
conda install -c anaconda basemap
conda install -c conda-forge basemap-data-hires
conda install -c anaconda netcdf4
conda install -c conda-forge wrf-python 
conda install -c anaconda numpy 
conda install -c anaconda pandas
conda install -c conda-forge progress 

To create videos, use:
sudo apt-get install ffmpeg
"""

import numpy                as     np
import matplotlib.pyplot    as     plt
from   mpl_toolkits.basemap import Basemap
from   roms_libs            import bbox2ij, shrink, rot2d
from   wrf                  import to_np, getvar, latlon_coords, extract_times
from   datetime             import datetime
import pandas               as     pd
from   progress.bar         import IncrementalBar
import matplotlib
import netCDF4
import os
import warnings
matplotlib.use('Agg')
warnings.filterwarnings("ignore")

# 2. Customizations.
roms_file = '/home/ueslei/Documents/COAWST_training/COAWST/Work/Sandy/Sandy_ocean_his.nc'
wrf_file  = '/home/ueslei/Documents/COAWST_training/COAWST/Work/Sandy/wrfout_d01_2012-10-28_12:00:00'
bbox          = [-82, -65, 28, 43]
initloop      = 0
zlev          = -1 # Last ROMS sigma layer corresponds to surface.
plot_var      = True 
plot_currents = False
plot_wind     = True
create_video  = True
ppt_fig       = False # If True, it will generate a figure with transparent background.
nc_roms       = netCDF4.Dataset(roms_file)
nc_wrf        = netCDF4.Dataset(wrf_file)
atemp         = nc_roms.variables['temp']
ntimes        = len(atemp)

print('Which contour? (1) Sea Surface Temperature (2) Heat Fluxes.')
contourf_var  = input()

# 3. Start looping through time
range_loop = [i for i in range(0,ntimes,1)]
bar        = IncrementalBar(max=len(range_loop))
for i in range_loop:
#for i in range(0,1):
    # 3.1. Get variables and their resources.
    #tvar        = nc_roms.variables['ocean_time']
    #timestr     = netCDF4.num2date(tvar[i], tvar.units).strftime('%b %d, %Y %H:%M')
    timestr1  = extract_times(nc_wrf,timeidx=None,meta=False,do_xtime=False)
    timestr11 = timestr1[i]
    timestr   = pd.to_datetime(timestr11, format="%b %d %Y %H:%M")

    if plot_wind==True:
        uvmet10 = getvar(nc_wrf, "uvmet10", i, units="m s-1")
        lats_wrf, lons_wrf = latlon_coords(uvmet10)
    if plot_currents==True:
        lon_rho     = nc_roms.variables['lon_rho'][:]
        lat_rho     = nc_roms.variables['lat_rho'][:]
        i0,i1,j0,j1 = bbox2ij(lon_rho,lat_rho,bbox)
        lon_var     = lon_rho[j0:j1, i0:i1]
        lat_var     = lat_rho[j0:j1, i0:i1]
        lon         = lon_rho[(j0+1):(j1-1), (i0+1):(i1-1)]
        lat         = lat_rho[(j0+1):(j1-1), (i0+1):(i1-1)]
        u           = nc_roms.variables['u'][i, zlev, j0:j1, i0:(i1-1)]
        v           = nc_roms.variables['v'][i, zlev, j0:(j1-1), i0:i1]
        mask        = 1 - nc_roms.variables['mask_rho'][(j0+1):(j1-1), (i0+1):(i1-1)]
        ang         = nc_roms.variables['angle'][(j0+1):(j1-1), (i0+1):(i1-1)] 
        u           = shrink(u, mask.shape) #Average U to rho points.
        v           = shrink(v, mask.shape) # Average V points to rho points.
        u, v        = rot2d(u, v, ang) # Rotate grid oriented U and V to East and West.
    if plot_var==True:
        if  contourf_var=='1':
            lon_rho     = nc_roms.variables['lon_rho'][:]
            lat_rho     = nc_roms.variables['lat_rho'][:]
            i0,i1,j0,j1 = bbox2ij(lon_rho,lat_rho,bbox)
            lon_var     = lon_rho[j0:j1, i0:i1]
            lat_var     = lat_rho[j0:j1, i0:i1]
            var         = nc_roms.variables['temp'][i, zlev,  j0:j1, i0:i1]
            clevs       = np.arange(13,29.1,0.01)
            ticks       = np.arange(min(clevs),max(clevs),2)  
            cmap        = plt.jet() #cmocean.cm.thermal      
        if contourf_var=='2': 
            latent   = nc_wrf.variables['LH'][i,:,:]
            sensible = nc_wrf.variables['HFX'][i,:,:]                
            var      = latent+sensible
            var1     = getvar(nc_wrf, "pw", i)
            lat_wrf, lon_wrf = latlon_coords(var1)      
            clevs    = np.arange(0,1011,10)
            ticks    = np.arange(min(clevs),max(clevs),200) 
            cmap     = plt.jet() # cmocean.cm.thermal    

    # 3.2. Create a figure.
    m = Basemap(projection='merc',llcrnrlat=bbox[2],urcrnrlat=bbox[3],llcrnrlon=bbox[0],urcrnrlon=bbox[1], lat_ts=30,resolution='i')
    fig = plt.figure(1,figsize=(10,8))
    plt.xlabel('Longitude'u' [\N{DEGREE SIGN}]',labelpad=13,size=7)
    plt.ylabel('Latitude'u' [\N{DEGREE SIGN}]',labelpad=24,size=7)
    ax   = fig.add_subplot(111)
    plt.title(timestr, fontsize=7)
        
    # 3.3. Add coastline, continents and lat/lon.
    m.drawparallels(np.arange(-90.,120.,2.), linewidth=0.00, color='black', labels=[1,0,0,1],labelstyle="N/S",fontsize=7)
    m.drawmeridians(np.arange(-180.,180.,2.), linewidth=0.00,color='black', labels=[1,0,0,1],labelstyle="N/S",fontsize=7)
    m.fillcontinents(color = '#ffffff')
    m.drawcountries(color = '#000000',linewidth=0.5)
    m.drawcoastlines(color = '#000000',linewidth=0.5)
    m.drawstates(color = '#000000',linewidth=0.5)       

    # 3.4. Plot Current vector.
    if plot_currents==True:
        x_rho, y_rho = m(lon,lat)
        nsub  = 5
        scale = 0.05  
        C = ax.quiver(x_rho[::nsub,::nsub],y_rho[::nsub,::nsub],u[::nsub,::nsub],v[::nsub,::nsub],alpha=0.5,scale=0.5/scale, zorder=1e35, width=0.0025,color='black',pivot='middle')
        qk = ax.quiverkey(C, .09, -0.17, 0.5, ' Sea Surface Currents\n 0.5 m.s⁻¹ ', coordinates='axes',color='#444444',labelsep=0.05, labelcolor='black',alpha=1,fontproperties={'size': '6'})

    # 3.5. Plot wind speed at 10 meters.
    if plot_wind==True:
        x, y    = m(to_np(lons_wrf), to_np(lats_wrf))
        spacing = 4
        scale   = 0.02
        W = ax.quiver(x[::spacing,::spacing], y[::spacing,::spacing], to_np(uvmet10[0,::spacing, ::spacing]),to_np(uvmet10[1,::spacing, ::spacing]),pivot='middle',scale=8/scale, zorder=1e35, width=0.005,color='gray',headlength=3, headaxislength=2.8 )
        wk = ax.quiverkey(W, 0.87, -0.17, 10, 'Wind Vector at 10 m\n 10 m.s⁻¹ ', coordinates='axes',color='black',labelsep=0.05, alpha=0.5,labelcolor='black',fontproperties={'size': '6'})

    # 3.7. Plot the desired countour variable and add some plot resources.
    if plot_var==True:
        if contourf_var=='1':
            h1  = m.contourf(lon_var, lat_var, var, clevs,latlon=True,cmap=cmap,extend="both")  
            cax = fig.add_axes([0.37, -0.002, 0.27, 0.025])     
            cb  = fig.colorbar(h1, cax=cax, orientation="horizontal",panchor=(0.5,0.5),shrink=0.3,ticks=ticks)
            cb.set_label(r'Sea Surface Temperature [$^\circ\!$C]', fontsize=6, color='0.2',labelpad=-1)
            cb.ax.tick_params(labelsize=6, length=2, color='0.2', labelcolor='0.2',direction='in') 
            cb.set_ticks(ticks)
        if contourf_var=='2':
            x1, y1 = m(to_np(lon_wrf), to_np(lat_wrf))
            h1  = ax.contourf(x1, y1, to_np(var), clevs,cmap=cmap,latlon=True,extend="both")  
            cax = fig.add_axes([0.37, 0.008, 0.27, 0.025])     
            cb  = fig.colorbar(h1, cax=cax, orientation="horizontal",panchor=(0.5,0.5),shrink=0.3,ticks=ticks)
            cb.set_label(r'Heat Fluxes [W m.s⁻²]', fontsize=6, color='0.2',labelpad=0)
            cb.ax.tick_params(labelsize=6, length=2, color='0.2', labelcolor='0.2',direction='in') 
            cb.set_ticks(ticks)            

    # 3.8. Save figures.
    if contourf_var=='1':
        try:
            os.makedirs("sst")
        except FileExistsError:
            pass
        if ppt_fig==True:
            plt.savefig('./sst/temp_{0:03d}.png'.format(i), transparent=True, bbox_inches = 'tight', pad_inches=0, dpi=250)
        if ppt_fig==False:
            plt.savefig('./sst/temp_{0:03d}.png'.format(i), transparent=False, bbox_inches = 'tight', pad_inches=0, dpi=250)        
        plt.clf()
    elif contourf_var=='2':
        try:
            os.makedirs("heat_fluxes")
        except FileExistsError:
            pass
        if ppt_fig==True:
            plt.savefig('./heat_fluxes/fluxes_{0:03d}.png'.format(i), transparent=True, bbox_inches = 'tight', pad_inches=0, dpi=250)
        if ppt_fig==False:
            plt.savefig('./heat_fluxes/fluxes_{0:03d}.png'.format(i), transparent=False, bbox_inches = 'tight', pad_inches=0, dpi=250)            
        plt.clf()   
        plt.clf()   
    bar.next()
bar.finish()
# 4. Create mp4 file from figures.
if create_video==True:
    cwd = os.getcwd()
    if contourf_var=='1':
        exists = os.path.isfile('./sst/wrf_roms_temp.mp4')
        if exists==True:
            os.system("rm -rf ./sst/wrf_roms_temp.mp4")
            os.system("ffmpeg -r 10 -pattern_type glob -i '"+cwd+"/sst/*.png' -c:v libx264 -pix_fmt yuv420p -crf 15 ./sst/wrf_roms_temp.mp4")
        else:
            os.system("ffmpeg -r 10 -pattern_type glob -i '"+cwd+"/sst/*.png' -c:v libx264 -pix_fmt yuv420p -crf 15 ./sst/wrf_roms_temp.mp4")
            print('Wish to delete the .png files? (1) Yes or (2) No.')
        removefiles = input()
        if removefiles=='1':
            os.system("rm -rf ./sst/*.png")
        if removefiles=='2':
            pass
    if contourf_var=='2':
        exists = os.path.isfile('./heat_fluxes/wrf_roms_flux.mp4')
        if exists==True:
            os.system("rm -rf ./heat_fluxes/wrf_roms_flux.mp4")
            os.system("ffmpeg -r 10 -pattern_type glob -i '"+cwd+"/heat_fluxes/*.png' -c:v libx264 -pix_fmt yuv420p -crf 15 ./heat_fluxes/wrf_roms_flux.mp4")
        else:
            os.system("ffmpeg -r 10 -pattern_type glob -i '"+cwd+"/heat_fluxes/*.png' -c:v libx264 -pix_fmt yuv420p -crf 15 ./heat_fluxes/wrf_roms_flux.mp4")
            print('Wish to delete the .png files? (1) Yes or (2) No.')
        removefiles = input()
        if removefiles=='1':
            os.system("rm -rf ./heat_fluxes/*.png")
        if removefiles=='2':
            pass
else:
    pass
