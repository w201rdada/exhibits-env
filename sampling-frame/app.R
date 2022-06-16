library(shiny)
library(ggplot2)
library(data.table)
library(magrittr)
library(cowplot)
library(withr)
library(forcats)
data.table::setDTthreads(1)

sz<-500
beta<-.5
exg<-3
with_seed(1,dat<-data.table(x=rnorm(sz))[,y:=beta*x+rnorm(sz)])
smp<<-dat[0]
fit<-lm(y ~ x, data = dat)
cf<-coef(fit)
bones<-data.table(x=dat[,seq(from=min(x),to=max(x),by=x %>% range %>% diff %>% {./6})])
bones[,y:=predict(fit,.SD)]
std<-summary(fit)$coefficients['x','Std. Error']*sqrt(sz)
dny<-seq(-2,2,length.out=100)
dnx<-dnorm(dny)*exg
crv<-mapply(x=bones$x,y=bones$y,ix=1:nrow(bones),function(x,y,ix) data.table(x=x+dnx,y=y+dny,ix=ix),SIMPLIFY = F) %>% rbindlist
crv[,ix:=factor(ix) %>% fct_rev]
crv %<>% .[!.(ix %>% levels %>% first),on='ix']

shinyApp(
	ui = fluidPage(
		plotOutput(
			outputId = "plot",
			brush = brushOpts(
				id = "plotBrush",fill = NA,stroke='blue',
				delay = 5000, direction = c('xy'),resetOnNew = F
			)
			,height='800px'
		),

	),
	server = function(input, output, session) {
		observeEvent(
			eventExpr = {
				input$plotBrush
			}
			,handlerExpr = {
				box<-isolate(input$plotBrush)
				# browser()
				smp<<-dat[which(between(x,box$xmin,box$xmax)&between(y,box$ymin,box$ymax)) %>% {sample(.,.5*length(.))}]
			}
			,ignoreNULL = T,ignoreInit = T,priority = 1
		)


		observeEvent(
			eventExpr = {
				input$plotBrush
			}
			,handlerExpr = {
				output$plot <- renderPlot({
					ggplot(dat, aes(x=x, y=y)) +
						geom_polygon(data=crv,aes(x=x,y=y,group=ix),fill='whitesmoke',color='gray',inherit.aes = F) +
						geom_vline(color='white',size=3,aes(xintercept=x),data=crv[,.(x=min(x)),by=ix]) +
						geom_point(size=2,shape=21) +
						geom_abline(intercept=cf[1],slope=cf[2],linetype='dashed') +
						coord_fixed() + xlim(-4,NA) + ylim(-4,4) + cowplot::theme_cowplot() +

						geom_smooth(data=smp,method='lm',linetype='solid',formula=y ~ x) +
						geom_point(data=smp,size=1.5,color='blue') +
						ggtitle(label = 'Population, sampling frame, and sample visualizer.'
										,subtitle = 'Brush to draw a sampling frame; a 50% sample will be randomly selected, and a linear model fit.\nThe dotted line represents the population fit. Normal curves represent conditional distributions of y along x.')
				})
			}
			,ignoreNULL = F,ignoreInit = F,priority = 0
		)
	}
)
