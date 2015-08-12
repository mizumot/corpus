library(shiny)
library(koRpus)

shinyServer(function(input, output){

	tagged.text <- reactive(tokenize(input$text, format="obj", lang="en"))
	hyphenated.text <- reactive({
			# set the next line to activate caching, if this application is run on a shiny server
			#set.kRp.env(hyph.cache.file=file.path("/var","shiny-server","cache","koRpus",paste("hyph.cache.",input$lang,".rdata", sep="")))
			hyphen(tagged.text(), quiet=TRUE)
		})
    
    output$word.list <- renderTable({
        x <- input$text
        x <- tolower(x) # 大文字を小文字に変換
        words <- unlist (strsplit (x, split = "[[:space:]]+|[[:punct:]]+"))
        Word <- words[words !=""]
        Word.freq <- as.data.frame(table (Word)) #頻度表の作成
        Word.sorted <- Word.freq[order(Word.freq$Freq, decreasing = TRUE), ]
        return(Word.sorted)
    })
    #output$word.list <- renderPrint({
    #word.list()
    #})
    
	output$letter.plot <- renderPlot(plot(tagged.text(), what="letters"))
	output$desc <- renderTable({
		basic.desc.data <- as.data.frame(describe(tagged.text())[c("all.chars","normalized.space","chars.no.space", "letters.only","lines",
			"punct","digits","words","sentences","avg.sentc.length","avg.word.length")])
		syll.desc.data <- as.data.frame(describe(hyphenated.text())[c("num.syll", "avg.syll.word")])
		colnames(basic.desc.data) <- c("All characters","Normalized space","Characters (no space)", "Characters (letters only)","Lines",
			"Punctuation","Digits","Words","Sentences","Avg. sentence length","Avg. word length")
		colnames(syll.desc.data) <- c("Syllables", "Avg. syllable per word")
		desc.data <- cbind(basic.desc.data, syll.desc.data)
		rownames(desc.data) <- c("Value")
		t(desc.data)
	})
	output$desc.lttr.disrib <- renderTable({
		t(describe(tagged.text())[["lttr.distrib"]])
	})
	output$syll.disrib <- renderTable({
		t(describe(hyphenated.text())[["syll.distrib"]])
	})

	LD.results <- reactive(lex.div(tagged.text(), segment=input$LD.segment, factor.size=input$LD.factor, min.tokens=input$LD.minTokens,
			rand.sample=input$LD.random, window=input$LD.window, case.sens=input$LD.caseSens, detailed=FALSE, char=c(), quiet=TRUE))
	output$lexdiv.sum <- renderTable({
		summary(LD.results())
	})
	output$lexdiv.res <- renderPrint({
		LD.results()
	})

	RD.results <- reactive(readability(tagged.text(), hyphen=hyphenated.text(), index=input$RD.indices, quiet=TRUE))
	output$readability.sum <- renderTable({
		summary(RD.results())
	})
	output$readability.res <- renderPrint({
		RD.results()
	})

})
