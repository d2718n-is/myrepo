library(readxl)
library(dplyr)
library('DT')
library(shiny)
library(googlesheets4)

# gs4_auth(cache = ".secrets", email = "doctden@gmail.com")
sheet_ID <- "https://docs.google.com/spreadsheets/d/1_ZmzrymvanAdRIpk5yvmdRko4W2LU7VOgNVvobkFAig/edit#gid=0"

qtyForDelivFun <- function(infoDt, DFin){
  vecPN <- which(DFin$`P/N`== DFin$`P/N`[infoDt$row])
  value <- trunc(ifelse(infoDt$value=="",0,infoDt$value))
  if(value < 0 | value ==""){
    infoDt$value <- 0
    value <- 0
  }
  val_adj <- 0
  for (vrbl in vecPN) {
    vec1 <- c(vrbl,
              grep("^Reserve$", colnames(DFin)),
              value
              )
    val_adj <- DFin[vrbl,"QtyForDelivery"] - value
    if(val_adj<0){
      vec2 <- c(vrbl,
                grep("^QtyForDelivery_adj$", colnames(DFin)),
                0)
      value <- abs(val_adj)
      
    } else {
      vec2 <- c(vrbl,
              grep("^QtyForDelivery_adj$", colnames(DFin)),
              val_adj)
      value <- 0
    }
    infoDt <- rbind(infoDt,vec1,vec2)
    
  }
  return(infoDt[-1,])
}

ui = fluidPage(
  fluidRow(column(
    12, h3("הזמנות"),
    DTOutput("Dtable"),
    actionButton('save', "Save")
  ))
)

server = function(input,output) {

  # loading data
  dataF <- read_sheet(sheet_ID, sheet = "DATA") %>% as.data.frame() %>%
    mutate_at(vars('Material','Paka','Department',
                   'Mashine','Extern_check','Shipment', 'Mat_size',
                   'Est_start', 'Remarks'),~as.character(.)) %>% 
    mutate_at(vars('QtyForDelivery_adj','Require_mat', 'Reserve', 'Length_mat',
                   'Length_part','Require_mat', 'Amount_paka','Made_qty',
                   'PiecesPerHour'),~as.numeric(.))
    
  
  
  d <- reactive(datatable(dataF,
                          editable = list(target = "cell",
                                          disable = list(columns = c(2,3))),
                          selection = 'none',
                          extensions = c('Buttons','FixedHeader','FixedColumns')
  ) %>% formatDate(1, "toLocaleDateString", params = list('fr-FR')
  ) %>% formatStyle('P/N', backgroundColor = 'lightgrey', fontWeight = 'bold'))

  options(DT.options = list(
    paging = TRUE,
    pageLength = 15,
    searching = TRUE,
    autoWidth =TRUE,
    ordering = TRUE,
    dom = 'Blfrtip',
    scrollX = TRUE,
    fixedHeader = TRUE,
    searchHighlight = TRUE,
    fixedColumns = list(leftColumns = 3),
    buttons = list('excel',list(extend = 'colvis', columns =I(':not(.noVis)') )),
    columnDefs = list(list(className = 'dt-center', targets ='_all'),
                      list(width = '85px', targets = 1),
                      list(width = '90px', targets = 2),
                      list(width = '300px', targets = 25),
                      list(width = '60px', targets = 10)
                      # list(targets = c(0,1,2,8,9,10,12,13,17,18,22,23,24), className = "noVis"),
                      # list(targets = which(!(1:ncol(dataF) %in% c(0,1,2,8,10,12,13,17,18,22,23,24))), visible = FALSE)
    )

  ))

  output$Dtable = renderDT(d(), server = TRUE  )

  proxy5 = dataTableProxy('Dtable')
  observeEvent(input$Dtable_cell_edit, {
    info = input$Dtable_cell_edit
    str(info)
    if(names(dataF)[info$col]=='Reserve') {
      info <- qtyForDelivFun(info,dataF)
    }
    dataF <<- editData(dataF, info)
    replaceData(proxy5, dataF,
                resetPaging = FALSE)
    # save(dataF, file = "data.Rda")
  })
  
  observeEvent(input$save,{
    sheet_write(data = dataF, sheet_ID, sheet = "DATA")
  })
}

shinyApp(ui, server)