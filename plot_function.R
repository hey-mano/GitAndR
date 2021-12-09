library(tidyverse)
library(glue)

gen_plot<-function(n_run=5,
                   n_samp=1000,
                   mean=0,
                   sd=1){
    
    rerun(n_run,rnorm(n_samp,mean,sd))%>%
        as_tibble_col()%>%
        mutate(ind=paste("RUN",
                         row_number(),sep = "_"))%>%
        unnest(cols = value)%>%
        ggplot(aes(value,fill=ind))+
        geom_density(alpha=1/n_run)+
        geom_vline(xintercept = mean,col="red",linetype="dashed")+
        theme_light()+
        theme(legend.position = "bottom")+
        labs(fill="",
             x="Value",
             y="Density",
             title =glue("Generated data with mean of {mean} & standard deviation of {sd}"),
             subtitle =glue("{n_samp} observations repeated {n_run} times"))
}
