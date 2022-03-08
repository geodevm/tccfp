#===============================================================================
# This script can be run from the python console within QGIS. Developed on
# QGIS version 3.22 and Python 3.9.5. This framework appends an area weighted
# average within a buffer around a point layer, and will work for any numeric
# field of a polygon shapefile. Setup for this script must be set within the
# QGIS Python environment, and details about how to configure that are included
# in the "pyqgis_env_setup.txt" document. This script appends population
# density.
# Import -----------------------------------------------------------------------
from qgis.core import *
# Supply path to qgis install location
QgsApplication.setPrefixPath("C:/OSGeo4W/apps/qgis", True)
# Create a reference to the QgsApplication. Setting the second argument to
# False disables the GUI.
qgs = QgsApplication([], False)
# Load providers
qgs.initQgis()
# Import more
from qgis.PyQt.QtCore import QVariant
import math
from osgeo import gdal
import processing
from processing.core.Processing import Processing
Processing.initialize()
# Data read and management -----------------------------------------------------
# Set root directory. THIS MUST BE CHANGED ON DIFFERENT MACHINES.
root = "C:/Users/mill8849/Documents/analysis_projects/tccfp/tccfp/data/"
# Read in gps_dat from csv
uri = "file:///" + root + \
"processed_data/gps_data.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % \
("UTF-8",",", "gps_utm_easting", "gps_utm_northing","epsg:26915")
gps_data = QgsVectorLayer(uri, "gps_data", "delimitedtext")
#Check if layer is valid
if not gps_data:
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(gps_data)
# Read in pop
pop_path = root + "gis_layers/population_density/population_density.shp"
pop_init = QgsVectorLayer(pop_path, "pop_init", "ogr")
if not pop_init:
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(pop_init)
# We should ensure that the geometry is not corrupted with a zero length buffer
buff_zero_params_pop = {
        'DISSOLVE' : False, 
        'DISTANCE' : 0, 
        'END_CAP_STYLE' : 0, 
        'INPUT' : pop_init, 
        'JOIN_STYLE' : 0, 
        'MITER_LIMIT' : 2, 
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
results = processing.run("native:buffer", buff_zero_params_pop)
pop = results['OUTPUT']
QgsProject.instance().removeMapLayer(pop_init)
# Read in outline
outline_path = root + "gis_layers/metro_outline/metro_outline.shp"
outline = QgsVectorLayer(outline_path, "metro_outline", "ogr")
if not outline:
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(outline)
# Pre-iteration processing -----------------------------------------------------
# We want to clip the gps data by the extent of the TCMA since that is our data
# extent for this spatial layer. We'll do a clip.
gps_params = {
    'INPUT' : gps_data,
    'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
    'OVERLAY' : outline
}
results = processing.run("qgis:clip", gps_params)
gps = results['OUTPUT']
# We also want to add the attribute field that we'll be iteratively adding
layer_provider = gps.dataProvider()
layer_provider.addAttributes([QgsField("pop_den", QVariant.Double)])
gps.updateFields()
# Remove the features that are no longer needed
QgsProject.instance().removeMapLayer(gps_data)
QgsProject.instance().removeMapLayer(outline)
# Buffer distance
# This is the baseline buffer. Most important value for the entire script 
# because it determines the entire outcome.
buff_dist = 20
# We also want to store the total area of our buffer, pi * radius ^ 2 
total_area = math.pi * (buff_dist ** 2)
# Retrieve features to be iterated over
features = gps.getFeatures()
# Set an iterator for adding observations to new field
i = 1
# Start loop -------------------------------------------------------------------
for feature in features:
    # Retrieve the geometry of the point observation
    geom = feature.geometry()
    # Geometries can't be buffered in the following algorithm, so it will need
    # to be converted into its own layer we'll call pts
    pts = QgsVectorLayer('Point?crs=epsg:26915', 'point' , 'memory')
    # Access the data provided for this layer
    prov = pts.dataProvider()
    # Create a feature called feat
    feat = QgsFeature()
    # Add the feature geometry to the feat geometry
    feat.setGeometry(QgsGeometry(geom))
    # Add the feat geometry to the pts data
    prov.addFeatures([feat])
    # Update canvas extent
    pts.updateExtents()
    # Add the layer to the Layers panel
    QgsProject.instance().addMapLayers([pts])
    # Create the buffer within which we'll create the weighted average
    buff_params = {
        'DISSOLVE' : False, 
        'DISTANCE' : buff_dist, 
        'END_CAP_STYLE' : 0, 
        'INPUT' : pts, 
        'JOIN_STYLE' : 0, 
        'MITER_LIMIT' : 2, 
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    results = processing.run("native:buffer", buff_params)
    buff_i = results['OUTPUT']
    # We don't need the points anymore, so remove those
    QgsProject.instance().removeMapLayer(pts)
    # Clip the pop layer within the buffer
    c_params = {
        'INPUT' : pop,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'OVERLAY' : buff_i
    }
    results = processing.run("qgis:clip", c_params)
    clip_int = results['OUTPUT']
    # Do a zero buffer to remove any corrupt geometries introduced
    buff_zero_params_c = {
        'DISSOLVE' : False, 
        'DISTANCE' : 0, 
        'END_CAP_STYLE' : 0, 
        'INPUT' : clip_int, 
        'JOIN_STYLE' : 0, 
        'MITER_LIMIT' : 2, 
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    processing.run("native:buffer", buff_zero_params_c)
    layer = results['OUTPUT']
    # Loop through the clipped layer to get the weighted average by summing with
    # the iterator j
    j = 0
    for clip in layer.getFeatures():        
        weight_value = clip.geometry().area() / total_area
        value = round((clip['pop_den'] * weight_value), 2)
        j = j + value
    # Remove the temporary clip layer
    QgsProject.instance().removeMapLayer(layer)
    # Assign the attribute value {column:attribute value}
    attr_value = {8:j}
    # Tell which row to fill in {row:{column:attribute value}}
    fill_in = {i:attr_value}
    layer_provider.changeAttributeValues(fill_in)
    i = i + 1
# End loop ---------------------------------------------------------------------
# Data management and export ---------------------------------------------------
# Commit changes to the movement data
gps.commitChanges()
# Remove the population dataset
QgsProject.instance().removeMapLayer(pop)
# Export the data as a csv
out_path = root + "processed_data/pop_appended.csv"
QgsVectorFileWriter.writeAsVectorFormat(gps, out_path, "utf-8", \
gps.crs(), "CSV")
# Close file
QgsProject.instance().removeMapLayer(gps)
# Finally, exitQgis() is called to remove the provider and layer registries
# from memory.
qgs.exitQgis()
#===============================================================================