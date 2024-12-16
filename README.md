# Polygon Data Handling
Code for handling various geospatial polygon data formats in multiple languages for use in ecological software.

## Background
Geospatial data are stored in a variety of formats and it can be helpful for a software tool to be flexible in which it can handle. Common formats are geoJSON, KML, and ESRI shapefiles and geodatabases. The code here aims to provide options for how users can use their spatial data, including error handling and feedback for the user.

## Functionality
### Uploading spatial data
Code related to uploading data should be able to identify (or restrict) incoming filetypes and react appropriately.

### Validating spatial data
Code should be able to validate incoming data, e.g., confirm that all required files in a shapefile are present. Ideally, the code would also provide feedback to the user regarding any detected issues.

### Selecting spatial data
Code should allow users to confirm which data to use. This is useful and helpful, but not strictly necessary if the tool has restrictions in place for the upload and validation stages which prevent the user from uploading multiple options.
