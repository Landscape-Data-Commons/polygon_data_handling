#### R Shiny ###################################################################
# This example is for web interfaces written in R using the package shiny. It's
# a "minimum viable app" for showing how the parts fit together. With shiny,
# there's very little in the way of client-side anything and so the server-side
# processing section is where most of the data handling takes place.

library(shiny)
library(tidyverse)

#### Interface #################################################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      ##### Polygon input handling ---------------------------------------------
      # The function to use is fileInput() which allows the user to upload a
      # file. In this case, the required filetype is ZIP because it can contain
      # any of the accepted spatial data formats, including multipart shapefiles.
      fileInput(inputId = "polygons",
                label = "Polygons ZIP file",
                multiple = FALSE,
                accept = ".zip"),
      
      # Options for selecting the uploaded polygons to use. This remains
      # unpopulated until the user uploads a ZIP containing at least one valid
      # shapefile or geodatabase.
      selectInput(inputId = "polygons_layer",
                  label = "Polygons name",
                  choices = c(""),
                  selected = ""),
      
      # Give the user an option to "repair" polygons when they're read in.
      # Lots of polygons have errors in them, especially if they've been through
      # a lot of geoprocessing steps. Some software can gloss over those issues
      # but for R most are cause for a hard crash. Repairing in this case is
      # simply buffering them all by a distance of 0 and hoping that fixes any
      # underlying geometry errors.
      checkboxInput(inputId = "repair_polygons",
                    label = "Repair polygons",
                    value = FALSE)
    ),
    mainPanel = mainPanel(
      # Display a map (once rendered)
      plotOutput(outputId = "map")
    )
  )
)

#### Server-side processing ####################################################
server <- function(input, output, session) {
  ##### Filesize limit ---------------------------------------------------------
  # Shapefiles can be large if they contain complicated polygons. The default
  # maximum uploaded filesize is often restrictive, but this allows the user to
  # upload files up to 100 MB. The function takes the filesize in bytes and
  # there are 1024 bytes in a kilobyte and 1024 kilobytes in a megabyte.
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  ##### Workspace reactive values list -----------------------------------------
  # Shiny uses reactive values to keep tabs on the status of objects and values
  # so that it can react appropriately. I find it's useful to have a workspace
  # list that objects and values can be stored in and referenced from throughout
  # the code because some objects are otherwise ephemeral. This can be very
  # helpful if you need to have part of the app reacting to an object that might
  # not have been created yet but will eventually. In this case, that's the
  # object mapping_polygons which will only exist after the user has uploaded
  # and selected their polygons. 
  workspace <- reactiveValues(temp_directory = tempdir(),
                              original_directory = getwd(),
                              mapping_polygons  = NULL)
  
  ##### Responding to input ----------------------------------------------------
  # When the user uploads a file through the part of the interface created by
  # fileInput(), the contents are stored in the input list under the inputId, in
  # this case called "polygons".
  # When input$polygons changes, it triggers this stretch of code to respond.
  observeEvent(eventExpr = input$polygons,
               handlerExpr = {
                 # Calls to message() only appear in the console and are useful
                 # for diagnostics and bug-hunting during development but will
                 # never be seen by the user.
                 message("Polygons file uploaded")
                 
                 # This is just in case the filetype isn't enforced by
                 # fileInput() and the user managed to upload something else.
                 polygon_upload_extension <- toupper(tools::file_ext(x = input$polygons$datapath))

                 polygons_are_zip <- polygon_upload_extension == "ZIP"
                 
                 # In the case that the user uploaded a file with the wrong
                 # extension, let them know.
                 if (!polygons_are_zip) {
                   ###### Example warning feedback to user ---------------------
                   # showNotification() creates a popup dialog overlayed on the
                   # tool interface. In this case, the text in the popup informs
                   # the user that the uploaded file needs to be a ZIP. It also
                   # will persist until dismissed by the user and is labeled as
                   # an error. The type argument is useful for CSS styling.
                   showNotification(ui = "Polygons must be uploaded as a zipped shapefile or zipped geodatabase.",
                                    duration = NULL,
                                    closeButton = TRUE,
                                    id = "polygons_zip_error",
                                    type = "error")
                 } else {
                   # If the user did upload a ZIP file, the tool continues to
                   # try to use it.
                   
                   ###### Unzipping --------------------------------------------
                   # The ZIP file needs to be unzipped before the contents can
                   # be used. This extracts the contents to a known location
                   # so that the next steps can happen.
                   message("Attempting to unzip file")
                   # Unzip with an OS-specific system call
                   # Setting the working directory
                   setwd(dirname(input$polygons$datapath))
                   # Passing this to the OS
                   system(sprintf("cd %s", dirname(input$polygons$datapath)))
                   # Just checking for debugging
                   message(getwd())
                   # The unzipping argument to pass to the OS
                   system(sprintf("unzip -u %s", input$polygons$datapath))
                   # Set the working directory back
                   setwd(workspace$original_directory)
                   
                   message("File unzipped")
                   
                   ###### Identifying ZIP contents -----------------------------
                   # Just because the user uploaded a ZIP file doesn't mean that
                   # they uploaded valid geospatial data. This stretch confirms
                   # what was actually uploaded.
                   
                   # First, we find all the files that were uploaded.
                   # NOTE: This looks recursively and returns the full filepath
                   # for every file. This is because it's very common for a user
                   # to have nested folders in an uploaded ZIP.
                   extracted_files <- list.files(dirname(input$polygons$datapath),
                                                 full.names = TRUE,
                                                 recursive = TRUE)
                   
                   # Identify which, if any, of the files have the file
                   # extension SHP. Every ESRI shapefile must include an SHP
                   # file and, because it's the file extension that looks the
                   # most like "shape" it's also the most likely for a user to
                   # have included even if they missed other compenents.
                   shp_indices <- grep(extracted_files,
                                        pattern = "\\.shp$",
                                        ignore.case = TRUE)
                   
                   ##### Checking shapefile completeness -----------------------
                   # As mentioned above, shapefiles are composed of at minimum
                   # four separate files which have the same filename but
                   # different extensions. It's very common for a user to miss
                   # one or more of these when zipping a shapefile, so this
                   # confirms which of the included SHP files have the rest too.
                   if (length(shp_indices) > 0) {
                     # These will be the extension-less filepaths and filenames
                     # for the shapefiles. We're keeping the filepaths intact
                     # because it's important that the components aren't in
                     # separate folders or something.
                     shp_paths <- stringr::str_extract(string = extracted_files[shp_indices],
                                            pattern = "^.+(?=\\.shp$)")
                     
                     # This produces a logical vector for the discovered SHP
                     # files which is TRUE when all required components are
                     # present and FALSE when one or more is missing.
                     has_all_files <- sapply(X = shp_paths,
                                             files = extracted_files,
                                             FUN = function(X, files) {
                                               # The required file extensions.
                                               required_filetypes <- c(".dbf",
                                                                       ".prj",
                                                                       ".shp",
                                                                       ".shx")
                                               # This creates the expected
                                               # filepaths for the required
                                               # components and confirms that
                                               # all of them exist. If any is
                                               # missing then this returns FALSE
                                               all(paste0(X,
                                                          required_filetypes) %in% files)
                                             })
                     
                     # Keep only the filepaths for SHP files that also have all
                     # other components.
                     shp_indices <- shp_indices[has_all_files]
                   }
                   
                   message(paste0("Found ",
                                  length(shp_indices),
                                  " shapefiles"))
                   # This is to register whether any SHP files were found for
                   # later use.
                   upload_has_shp <- length(shp_indices) > 0
                   
                   # Identify if any geodatabases were extracted.
                   gdb_indices <- grepl(extracted_files,
                                        pattern = "\\.gdb",
                                        ignore.case = TRUE)
                   
                   # Geodatabases look like folders to the OS and the search for
                   # extracted files was recursive, so the gdb_indices will
                   # contain any GDBs and also every single object inside them.
                   # This extracts just the filepath up to the GDB file
                   # extension and keeps only the unique values.
                   gdb_paths <- unique(stringr::str_extract(string = extracted_files[gdb_indices],
                                                            pattern = ".*(?=\\.gdb/)"))
                   # This adds the file extension back to the files that were
                   # found and then removes any that were somehow only the file
                   # extension.
                   gdb_paths <- paste0(gdb_paths,
                                       ".gdb")
                   gdb_paths <- gdb_paths[!(gdb_paths %in% c(".gdb"))]
                   
                   message(paste0("Found ",
                                  length(gdb_paths),
                                  " geodatabases"))
                   upload_has_gdb <- length(gdb_paths) > 0
                   
                   ##### Selecting the correct data ----------------------------
                   # In this case, the user is going to be given an option if
                   # there were multiple valid spatial inputs provided. This
                   # could simply be handled by taking the first one and telling
                   # the user that that's what happened, but this makes it much
                   # more likely that the user will succeed at uploading and
                   # using the intended polygons.
                   
                   # I've made the choice to keep it relatively simple by
                   # prioritizing feature classes in geodatabases. If there was
                   # a geodatabase, the user will be able to select a feature
                   # class to use from within it. If there were multiple
                   # geodatabases, the user will be able to select a feature
                   # class from within the first one identified.
                   # If no geodatabases were found, the user will be able to
                   # select one of any valid shapefiles found.
                   if (upload_has_gdb) {
                     # This value is used to help guide the reading process
                     # later in a difference observeEvent() call.
                     workspace$polygon_filetype <- "gdb"
                     message("Working from extracted geodatabase")
                     # If there's more than one geodatabase, just use the first
                     # but warn the user
                     if (length(gdb_paths) > 1) {
                       message("Multiple GDBs detected. Using 'first' one")
                       showNotification(ui = "More than one geodatabase found in ZIP file. Please upload one at a time.",
                                        duration = NULL,
                                        closeButton = TRUE,
                                        type = "warning",
                                        id = "multiple_gdb_warning")
                     }
                     
                     # Make sure that the current path is the first identified
                     # geodatabase.
                     current_gdb_path <- gdb_paths[1]
                     
                     # Due to quirks in shiny, it makes sense to store the same
                     # value in a list which is accessible to any server-side
                     # code because the object current_gdb_path only exists
                     # within the current observeEvent() call.
                     workspace$gdb_filepath <- current_gdb_path
                     
                     # At this point, no data have been read out of the
                     # geodatabase, so this is the first point where the
                     # validity of the file is put to the test. The package sf
                     # contains a suite of functions for dealing with spatial
                     # data, including st_layers() which will list the layers
                     # or feature classes in the geodatabase.
                     available_polygons <- sf::st_layers(dsn = current_gdb_path)$name
                     message(paste0("Available layers in GDB are: ",
                                    paste(available_polygons,
                                          collapse = ", ")))
                     message("Updating selectInput(inputId = 'polygons_layer')")
                     
                     # Once the available feature classes are identified, they
                     # can be listed for the user in the interface. This updates
                     # the input called "polygons_layer" so that the options in
                     # the dropdown are the feature class names and the current
                     # selection is the first listed.
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else if (upload_has_shp) {
                     # If there were no geodatabases and there were shapefiles,
                     # we'll do a similar thing to above with the geodatabase.
                     
                     # This value is used to help guide the reading process
                     # later in a difference observeEvent() call.
                     workspace$polygon_filetype <- "shp"
                     
                     message("Working with extracted shapefile(s)")
                     
                     # The user needs to be able to choose the shapefile, but
                     # it makes no sense to make them do it with full filepaths
                     # so this puts human-friendly names on all of them using
                     # the filename without the extension.
                     available_polygons <- extracted_files[shp_indices]
                     shp_filenames <- gsub(x = basename(available_polygons),
                                           pattern = "\\.shp$",
                                           replacement = "",
                                           ignore.case = TRUE)
                     names(available_polygons) <- shp_filenames
                     
                     # In shiny inputs, named values like the available_polygons
                     # vector appear using the names for the user but are 
                     # recognized as the actual values by the code.
                     updateSelectInput(session = session,
                                       inputId = "polygons_layer",
                                       choices = available_polygons,
                                       selected = available_polygons[1])
                   } else {
                     showNotification(ui = "Uploaded file does not appear to contain either a valid shapefile or geodatabase.",
                                      duration = NULL,
                                      closeButton = TRUE,
                                      type = "error",
                                      id = "empty_upload_error")
                   }
                 }
               })
  
  ##### Reading polygons -------------------------------------------------------
  # This waits until the user updates their selection of polygons from what they
  # upload. Any time that selection changes (including when the server changes
  # the options to reflect what was uploaded), this fires and reads the data
  # into the server's working environment as the appropriate object type.
  observeEvent(eventExpr = {input$polygons_layer},
               handlerExpr = {
                 message("input$polygons_layer has updated")
                 message(workspace$polygon_filetype)
                 if (input$polygons_layer != "") {
                   message("Reading in polygons")
                   if (workspace$polygon_filetype == "gdb") {
                     message("Reading from GDB")
                     workspace$mapping_polygons <- sf::st_read(dsn = workspace$gdb_filepath,
                                                               layer = input$polygons_layer)
                   } else if (workspace$polygon_filetype == "shp") {
                     message("Reading in SHP")
                     workspace$mapping_polygons <- sf::st_read(dsn = input$polygons_layer)
                   }
                   message("Making sure the polygons are in NAD83")
                   workspace$mapping_polygons <- sf::st_transform(workspace$mapping_polygons,
                                                                  crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs +type=crs")
                   
                   # If they've asked us to "repair" polygons, buffer by 0
                   if (input$repair_polygons) {
                     message("Attempting to repair polygons by buffering by 0")
                     workspace$mapping_polygons <- sf::st_buffer(x = workspace$mapping_polygons,
                                                                 dist = 0)
                   }
                   workspace$mapping_polygons <- dplyr::mutate(.data = workspace$mapping_polygons,
                                                               internal_uid = dplyr::row_number())
                 }
               })
  
  observeEvent(eventExpr = workspace$mapping_polygons,
               handlerExpr = {
                 message("Something changed for mapping purposes.")
                 
                 if (!is.null(workspace$mapping_polygons)) {
                   map <- ggplot() +
                     geom_sf(data = workspace$mapping_polygons,
                             aes(fill = internal_uid))
                   
                   message("Rendering map")
                   output$map <- renderPlot(expr = {map})
                   message("Map rendered")
                 } else {
                   message("No polygons to render")
                   output$map <- NULL
                 }
                 
               })
}

# Run the application 
shinyApp(ui = ui, server = server)