#===============================================================================
# This script can be run from the python console within QGIS. Developed on
# QGIS version 3.22 and Python 3.9.5. This framework appends an area weighted
# average within a buffer around a point layer, and will work for any numeric
# field of a raster dataset. Setup for this script must be set within the QGIS
# Python environment, and details about how to configure that are included in
# the "pyqgis_env_setup.txt" document.
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
from qgis.PyQt.QtCore import (
    QVariant,
)
import math
import processing
from processing.core.Processing import Processing
Processing.initialize()
# Data read and management -----------------------------------------------------
# Set root directory. THIS MUST BE CHANGED ON DIFFERENT MACHINES.
root = "C:/Users/mill8849/Documents/analysis_projects/tccfp/tccfp/data/"
# Read in gps_dat from csv
uri = "file:///" + root + \
"processed_data/movement_data.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % \
("UTF-8",",", "gps_utm_easting", "gps_utm_northing","epsg:26915")
gps_data = QgsVectorLayer(uri, "gps_data", "delimitedtext")
#Check if layer is valid
if not gps_data:
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(gps_data)
# Read in imp
imp_path = root + "gis_layers/impervious/TCMA_Impervious_2000.tif"
imp = QgsRasterLayer(imp_path, "imp")
if not imp:
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(imp)
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
layer_provider.addAttributes([QgsField("population", QVariant.Double)])
gps.updateFields()
# Remove the features that are no longer needed
QgsProject.instance().removeMapLayer(gps_data)
QgsProject.instance().removeMapLayer(outline)
## Iteration section
# Buffer distance b
# This is the baseline buffer. Most important value for the entire script
# because it determines the entire outcome. It's buffer b because it comes
# second during iteration.
buff_dist_b = 20
# Buffer distance a
# To get the centroid of the pixels (which is the only way they can be
# with polygons), you have to take the distance from the corner of the pixel to
# the center of the pixel and add it to the baseline buffer dimension. Using the
# pythagorean theorem, we know that this is sqrt((pixel l /2)^2 *2).
buff_dist_a = math.sqrt(((imp.rasterUnitsPerPixelX() / 2) ** 2) * 2) + \
buff_dist_b
# We also want to store the total area of our buffer, pi * r ^ 2
total_area = math.pi * (buff_dist_b ** 2)
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
    # Create the buffer which we'll use to mask the raster layer. Since the
    # raster centroid is what's intersected as explained above, this is where
    # that value is used.
    buff_a_params = {
        'DISSOLVE' : False,
        'DISTANCE' : buff_dist_a,
        'END_CAP_STYLE' : 0,
        'INPUT' : pts,
        'JOIN_STYLE' : 0,
        'MITER_LIMIT' : 2,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    results = processing.run("native:buffer", buff_a_params)
    buff_a_i = results['OUTPUT']
    # Create the buffer which will be used to clip the mask layer. For some
    # reason related to the way rasters are referenced after being masked, there
    # is a constant shift of 8.9 meters west, and 15 meters north.
    # Translate original point, generating pts_2:
    geom.translate(-8.900000000, 15)
    pts_2 = QgsVectorLayer('Point?crs=epsg:26915', 'point' , 'memory')
    prov_2 = pts_2.dataProvider()
    feat_2 = QgsFeature()
    feat_2.setGeometry(QgsGeometry(geom))
    prov_2.addFeatures([feat_2])
    # Update extent of the layer
    pts_2.updateExtents()
    # Add the layer to the Layers panel
    QgsProject.instance().addMapLayers([pts_2])
    buff_b_params = {
        'DISSOLVE' : False,
        'DISTANCE' : buff_dist_b,
        'END_CAP_STYLE' : 0,
        'INPUT' : pts_2,
        'JOIN_STYLE' : 0,
        'MITER_LIMIT' : 2,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    results = processing.run("native:buffer", buff_b_params)
    buff_b_i = results['OUTPUT']
    QgsProject.instance().removeMapLayer(pts)
    QgsProject.instance().removeMapLayer(pts_2)
   # Clip the raster using the mask layer.
    icr_params = {
        'ALPHA_BAND' : False,
        'CROP_TO_CUTLINE' : True,
        'DATA_TYPE' : 0,
        'EXTRA' : '',
        'INPUT' : imp,
        'KEEP_RESOLUTION' : True,
        'MASK' : buff_a_i,
        'MULTITHREADING' : False,
        'NODATA' : None,
        'OPTIONS' : '',
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SET_RESOLUTION' : False,
        'SOURCE_CRS' : None,
        'TARGET_CRS' : None,
        'X_RESOLUTION' : None,
        'Y_RESOLUTION' : None
    }
    results = processing.run("gdal:cliprasterbymasklayer", \
    icr_params)
    imp_clip_rast = results['OUTPUT']
    # Polygonize the masked raster.
    icv_params = {
        'BAND' : 1,
        'EIGHT_CONNECTEDNESS' : True,
        'EXTRA' : '',
        'FIELD' : 'DN',
        'INPUT' : imp_clip_rast,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT
    }
    results = processing.run("gdal:polygonize", icv_params)
    imp_clip_vec_int = results['OUTPUT']
    # Do a zero length buffer in case of corrupted geometry.
    buff_zero_params_vec = {
        'DISSOLVE' : False,
        'DISTANCE' : 0,
        'END_CAP_STYLE' : 0,
        'INPUT' : imp_clip_vec_int,
        'JOIN_STYLE' : 0,
        'MITER_LIMIT' : 2,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    results = processing.run("native:buffer", buff_zero_params_vec)
    imp_clip_vec = results['OUTPUT']
    # Clip the polygonized raster to the main buffer.
    ic_params = {
        'INPUT' : imp_clip_vec,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'OVERLAY' : buff_b_i
    }
    results = processing.run("qgis:clip", ic_params)
    imp_clip_int = results['OUTPUT']
    # Do a zero length buffer in case of corrupted geometry.
    buff_zero_params_imp = {
        'DISSOLVE' : False,
        'DISTANCE' : 0,
        'END_CAP_STYLE' : 0,
        'INPUT' : imp_clip_int,
        'JOIN_STYLE' : 0,
        'MITER_LIMIT' : 2,
        'OUTPUT' : QgsProcessing.TEMPORARY_OUTPUT,
        'SEGMENTS' : 5
    }
    results = processing.run("native:buffer", buff_zero_params_imp)
    layer = results['OUTPUT']
    # Loop through the clipped layer to get the weighted average by summing with
    # the iterator j
    j = 0
    for clip in layer.getFeatures():
        weight_value = clip.geometry().area() / total_area
        value = round((clip['DN'] * weight_value), 2)
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
# Remove the impervious dataset
QgsProject.instance().removeMapLayer(imp)
# Export the data as a csv
out_path = root + "processed_data/imp_appended.csv"
QgsVectorFileWriter.writeAsVectorFormat(gps, out_path, "utf-8", \
gps.crs(), "CSV")
# Close file
QgsProject.instance().removeMapLayer(gps)
# Finally, exitQgis() is called to remove the provider and layer registries
# from memory.
qgs.exitQgis()
#===============================================================================