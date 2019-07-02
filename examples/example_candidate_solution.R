
dir <- dirname(sys.frame(1)$ofile);
source(file.path(dir, "..", "R", "utils.R"));
source(file.path(dir, "..", "R", "graphics.R"));

if(exists("config")) {
  .old.config <- config;
  config$graphics.ext <- "svg";
} else {
  config <- list(logger=function(...) { }, graphics.ext="svg");
  .old.config <- NULL;
}

.graphic(config, file.path(dir, .graphics.name(config, "jssp_example_candidate_solution")), 4, 3, {

  if(!(require("plotteR"))){
    if(!(require("devtools"))){
      install.packages("devtools");
      library("devtools");
    };
    devtools::install_github("thomasWeise/plotteR");
    library("plotteR");
  };
  plot.gantt(list(
    list( list(job=0L,start=0L,end=10L)
          ,list(job=1L,start=20L,end=30L)
          ,list(job=3L,start=155L,end=175L)
          ,list(job=2L,start=175L,end=185L)
    )
    ,list( list(job=1L,start=0L,end=20L)
           ,list(job=2L,start=30L,end=50L)
           ,list(job=0L,start=50L,end=70L)
           ,list(job=3L,start=175L,end=190L)
    )
    ,list( list(job=2L,start=0L,end=30L)
           ,list(job=0L,start=70L,end=90L)
           ,list(job=1L,start=90L,end=140L)
           ,list(job=3L,start=140L,end=155L)
    )
    ,list( list(job=1L,start=30L,end=60L)
           ,list(job=3L,start=60L,end=90L)
           ,list(job=0L,start=90L,end=130L)
           ,list(job=2L,start=130L,end=170L)
    )
    ,list( list(job=3L,start=0L,end=50L)
           ,list(job=2L,start=50L,end=62L)
           ,list(job=0L,start=130L,end=140L)
           ,list(job=1L,start=140L,end=170L)
    )
  ), prefix.job="", xlab=NA, ylab=NA);

  });
