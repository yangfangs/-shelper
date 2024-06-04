#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(knitr)
library(sangerseqR)
library(future)
library(promises)
library(future.apply)
plan(multisession)  # 设置异步执行计划为多会话

setwd("/srv/shiny-server/shelper")
source('polypeakfunctions.R')



shinyServer(function(input, output,session) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~primer~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # primer_reactive <- eventReactive(input$submit_primer, {
  #   primerRes(input$location_primer,input$genomeVersion2)
  # })
  
  
  
  
  observe({
    req(input$example1)  # 确保在使用之前输入已经被提供
    if(input$example1) {
      updateTextInput(session, "location_primer", value = "13-32914859-G-GA")
      # print(input$location_primer)
    }
  })
  
  
  
  # Define the reactive value that captures the result of future computation
  primer_reactive <- eventReactive(input$submit_primer, {
    location_primer <- input$location_primer  # Assuming input UI element exists
    genomeVersion2 <- input$genomeVersion2    # Assuming input UI element exists
    
    future({
      # Simulating a function that fetches primer data
      primerRes(location_primer, genomeVersion2)
    }) %...!% {
      # Catching exceptions from the future computation
      warning("Error in future: ", .)
      NULL
    }
  })
  
  # Observing the primer submission button
  observeEvent(input$submit_primer, {
    # 判断 input$location_primer 是否为空
    if (is.null(input$location_primer) || input$location_primer == "") {
      showModal(modalDialog(
        title = "Warning:",
        "Please input genome location",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    
    
    updateTabsetPanel(session, "mainTabset2", selected = "primer")
    req(primer_reactive())  # Ensure primer_reactive() is not NULL or execution stops
    
    # Handle the results once the future resolves
    primer_reactive() %...>% {
      if (!is.data.frame(.)) {
        warning("result_primers is not a data frame")
        return(NULL)
      }
      
      split_primers <- split(., (seq(nrow(.)) - 1) %/% 2)
      
      output$tables <- renderUI({
        table_output_list <- lapply(seq_along(split_primers), function(i) {
          wellPanel(
            h4(paste("Primer pair", i)),
            DT::dataTableOutput(outputId = paste0("table_", i))
          )
        })
        do.call(tagList, table_output_list)
      })
      
      lapply(seq_along(split_primers), function(i) {
        output[[paste0("table_", i)]] <- DT::renderDataTable({
          split_primers[[i]]
        }, options = list(paging = FALSE, searching = FALSE, info = FALSE, lengthChange = FALSE))
      })
    }
  })
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~example for validate~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # 反应性值，存储textAreaInput的内容
    textContent <- reactiveVal("")
    
    observeEvent(input$example, {
      # 当example复选框被选中时
      if (input$example) {
        # 定义示例数据
        exampleText <- "17-41245245-CT-C,example1.ab1\n13-32893229-G-A,example2.ab1\n13-32914859-G-GA,example3.ab1\n13-20763530-CACACGTTCTTGCAGCC-C,example4.ab1\n13-20763210-CGTT-CGTTCGTT,example5.ab1"
        # 更新反应性值
        textContent(exampleText)
      }
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~variant upload~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 用于存储数据的反应值
    values <- reactiveValues(data = NULL, showTable = FALSE)
    
    observeEvent(input$submit, {
      

      
      if(input$example) {
        updateTextAreaInput(session, "textData", value = textContent())
        values$showTable <- TRUE
        updateTabsetPanel(session, "mainTabset", selected = "Results")
        # 加载示例数据
        exampleData <- data.frame(GenomicCoordinate = c("17-41245245-CT-C", "13-32893229-G-A","13-32914859-G-GA",'13-20763530-CACACGTTCTTGCAGCC-C',"13-20763210-CGTT-CGTTCGTT"),
                                  FileName = c("example1.ab1", "example2.ab1","example3.ab1","example4.ab1","example5.ab1"),
                                  FilePath = c("example1.ab1", "example2.ab1","example3.ab1","example4.ab1","example5.ab1"))
        values$data <- exampleData
        
      } else{
        # 判断 input$location_primer 是否为空
        if (is.null(input$textData) || input$textData == "") {
          showModal(modalDialog(
            title = "Warning:",
            "please input vilidate location and file name",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        
        
        # selectedGenomeVersion <- input$genomeVersion
        #切换标签
        updateTabsetPanel(session, "mainTabset", selected = "Results")
        #控制table显示
        values$showTable <- TRUE
        # 确保输入了文本数据
        if (nchar(input$textData) == 0) {
          values$data <- data.frame(Error = "未输入文本数据")
        } else {
          # 解析文本数据
          lines <- strsplit(input$textData, "\n")[[1]]
          parsedData <- data.frame(matrix(ncol = 2, nrow = length(lines)))
          colnames(parsedData) <- c("GenomicCoordinate", "FileName")
          
          for (i in 1:length(lines)) {
            parsedData[i,] <- strsplit(lines[i], ",")[[1]]
          }
          
          # 将数据与文件名结合（如果已上传文件）
          if (!is.null(input$file1)) {
            # 创建一个以上传文件名为键，以文件路径为值的映射
            fileMap <- setNames(input$file1$datapath, input$file1$name)
            
            # 将文件路径添加到解析数据中
            parsedData$FilePath <- fileMap[parsedData$FileName]
            
            values$data <- parsedData
            
          } else {
            values$data <- data.frame(Error = "未上传文件")
          }
        }
      }
      
      # 更新表格输出
      
      output$table1 <- renderTable({
        values$data
      })
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    data_reactive <- reactive({
      # shinyjs::show(id = "loading")  # 显示加载指示器

      req(values$data)
      values_data <- values$data
      input_genomeVersion <- input$genomeVersion

      result <- future({

        if (is.null(values_data) || nrow(values_data) == 0) {
          return(data.frame())  # 直接返回空的数据框
          # shinyjs::hide("loading") 
        }
        pickchromatogram2(values_data, input_genomeVersion)
      })

      result %...>% {
            result <- .
            # shinyjs::hide("loading") 
            return(result)
      }
    })
    
  
    output$table <- renderDataTable({
      
      
      req(data_reactive())
      data_reactive() %...>% {
      
      data <- .$res_table  # 获取最新的数据
      
      # 转换Figure列和Check列
      data$Figure <- sapply(data$Figure, function(i) {
        as.character(actionButton(inputId = paste0("button", i), label = "Show"))
      })
      data$Check <- sapply(data$Check, function(check) {
        if (check) {
          as.character(tags$span("\u2714", style = "color:green;"))
        } else {
          as.character(tags$span("\u2718", style = "color:red;"))
        }
      })
      
      datatable(data, escape = FALSE, options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }'),
        columnDefs = list(list(targets = 6, visible = TRUE))
      ))
      
      }
    })
    
    
    showTags <- reactiveVal(FALSE)
    
    observe({
      req(data_reactive())
      data_reactive() %...>% {
        valid_data <- .
        if (!is.null(valid_data) && nrow(valid_data$res_table) > 0) {
          lapply(seq_len(nrow(valid_data$res_table)), function(i) {
            observeEvent(input[[paste0("button", i)]], {
              showTags(TRUE)
              
              output$fig_text <- renderPlot(chromatogram(valid_data$chromatogram_pick[[i]], width = 25, height = 3, trim5 = 0, trim3 = 0,showcalls = "both"))
              output$refseq <- renderText(valid_data$refseq[[i]])
              output$altseq <- renderText(valid_data$altseq[[i]])
              output$alignment <- renderText(
                gsub(pattern="^.*#={39}(.+?)#-{39}.*$",
                     replacement="\\1",
                     x=valid_data$alignment[[i]])
              )
              output$header <- renderText(
                gsub(pattern="(^.+)#\\n#\\n#={39}.+$",
                     replacement="\\1",
                     x=valid_data$alignment[[i]])
              )
            })
          })
        }
      }
    })
    
    

    # 根据 showTags 的值动态生成UI
    output$dynamicUI <- renderUI({
      if(showTags()) {  # 如果 showTags 为 TRUE
        tagList(
          tags$h4("Alignment"), 
          verbatimTextOutput('alignment'),
          tags$h4("Reference Sequence"), 
          verbatimTextOutput('refseq'),
          tags$h4("Alternate Allele"),
          verbatimTextOutput('altseq'),
          tags$h4("Alignment Header"), 
          verbatimTextOutput('header')
        )
      }
    })
    # 根据 showTable 的值动态生成 dataTableOutput
    output$dynamicTable <- renderUI({
      if(values$showTable) {  
        tagList(
          tags$br(style="clear:both"),
          div(
            id = "download-buttons",
            downloadButton("downloadTable", "Download Table"),
            downloadButton("downloadPlots", "Download Plots"),
            style = "text-align: right;"  # 添加样式
          ),
          tags$h4("Viliation Result"),
          tags$br(),
          dataTableOutput("table")
        )
      } else {
        tags$p("Check results will be shown here when a valid sequencing file has been uploaded.")
      }
    })
      
    
    output$downloadTable <- downloadHandler(
      filename = function() {
        paste("table-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        # 获取表格数据
        table_data <- data_reactive()$res_table
        # 如果数据不为空，则写入 CSV 文件
        if (!is.null(table_data)) {
          write.csv(table_data, file, row.names = FALSE)
        }
      }
    )
 
    save_plot <- function(data, folder, filename) {
      # 确保文件夹存在
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
      
      # 完整的文件路径
      full_filename <- file.path(folder, filename)
      
      png(full_filename, width = 25*72, height = 3*72) # 72 是每英寸的像素数
      chromatogram(data, width = 25, height = 3, trim5 = 0, trim3 = 0, showcalls = "both")
      dev.off()
    }
    
    
    output$downloadPlots <- downloadHandler(
      filename = function() {
        paste("Chromatogram-", Sys.Date(), ".zip", sep="")
      },
      content = function(zip_filename) {
        # 创建一个临时目录来存储 "Chromatogram" 文件夹
        temp_dir <- tempdir()
        chromatogram_folder <- file.path(temp_dir, "Chromatogram")
        
        # 为每个图表生成一个文件
        for (i in seq_along(data_reactive()$chromatogram_pick)) {
          plot_filename <- paste0("plot_", i, ".png")
          save_plot(data_reactive()$chromatogram_pick[[i]], chromatogram_folder, plot_filename)
        }
        
        # 将 "Chromatogram" 文件夹打包成 ZIP 文件
        # 注意：'zip' 命令的工作目录设置为 'temp_dir'
        setwd(temp_dir)
        zip(zip_filename, "Chromatogram")
        setwd(normalizePath(tempdir(), winslash = "/", mustWork = FALSE))  # 恢复原始工作目录
      }
    )
    

}
)
