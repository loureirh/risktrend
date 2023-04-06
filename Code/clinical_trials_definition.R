trial_list <- list(
    ## Trial 1 - KEYNOTE-187
    list(
        Trial_name = "KEYNOTE-189",
        Var1 = "Platinum+Pembrolizumab+Pemetrexed",
        Var2 = "Platinum+Pemetrexed",
        linenumber = 1,
        Var1_LoT = c(
            "Carboplatin,Pembrolizumab,Pemetrexed",
            "Cisplatin,Pembrolizumab,Pemetrexed"
        ),
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Cisplatin,Pemetrexed"
        )
    ),
    ## Trial 2 - KEYNOTE-024
    list(
        Trial_name = "KEYNOTE-024",
        Var1 = "Pembrolizumab",
        Var2 = "Platinum-doublet (5 types)",
        linenumber = 1,
        Var1_LoT = c(
            "Pembrolizumab"
        ), 
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Cisplatin,Pemetrexed",
            "Carboplatin,Gemcitabine",
            "Cisplatin,Gemcitabine",
            "Carboplatin,Paclitaxel"
        )
    ),
    ## Trial 3 - KEYNOTE-042
    list(
        Trial_name = "KEYNOTE-042",
        Var1 = "Pembrolizumab",
        Var2 = "Platinum-doublet (2 types)",
        linenumber = 1,
        Var1_LoT = c(
            "Pembrolizumab"
        ),
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Carboplatin,Paclitaxel"
        )
    ),
    ## Trial 4 - KEYNOTE-407
    list(
        Trial_name = "KEYNOTE-407",
        Var1 = "Carboplatin+Paclitaxel+Pembrolizumab",
        Var2 = "Carboplatin+Paclitaxel",
        linenumber = 1,
        Var1_LoT = c(
            "Carboplatin,Paclitaxel,Pembrolizumab",
            "Carboplatin,Paclitaxel Protein-Bound,Pembrolizumab"
        ),
        Var2_LoT = c(
            "Carboplatin,Paclitaxel",
            "Carboplatin,Paclitaxel Protein-Bound"
        )
    ),
    ## Trial 5 - PRONOUNCE
    list(
        Trial_name = "PRONOUNCE",
        Var1 = "Carboplatin+Pemetrexed",
        Var2 = "Bevacizumab+Carboplatin+Paclitaxel",
        linenumber = 1,
        Var1_LoT = c(
            "Carboplatin,Pemetrexed"
        ),
        Var2_LoT = c(
            "Bevacizumab,Carboplatin,Paclitaxel"
        )
    ),
    ## Trial 6 - PointBreak
    list(
        Trial_name = "PointBreak",
        Var1 = "Bevacizumab+Carboplatin+Pemetrexed",
        Var2 = "Bevacizumab+Carboplatin+Paclitaxel",
        linenumber = 1,
        Var1_LoT = c(
            "Bevacizumab,Carboplatin,Pemetrexed"
        ),
        Var2_LoT = c(
            "Bevacizumab,Carboplatin,Paclitaxel"
        )
    ),
    ## Trial 7 - PROFILE 1014
    list(
        Trial_name = "PROFILE 1014",
        Var1 = "Crizotinib",
        Var2 = "Carboplatin-Cisplatin+Pemetrexed",
        linenumber = 1,
        Var1_LoT = c(
            "Crizotinib"
        ),
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Cisplatin,Pemetrexed"
        )
    ),
    ## Trial 8 - FLAURA
    list(
        Trial_name = "FLAURA",
        Var1 = "Osimertinib",
        Var2 = "Erlotinib-Gefitinib",
        linenumber = 1,
        Var1_LoT = c(
            "Osimertinib"
        ),
        Var2_LoT = c(
            "Erlotinib",
            "Gefitinib"
        )
    ),
    ## Trial 9 - LUX-Lung 3
    list(
        Trial_name = "LUX-Lung 3+6",
        Var1 = "Afatinib",
        Var2 = "Platinum-doublet (3 types)",
        linenumber = 1,
        Var1_LoT = c(
            "Afatinib"
        ),
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Cisplatin,Pemetrexed",
            "Cisplatin,Gemcitabine"
        )
    ),
    ## Trial 10 - NCT00540514
    list(
        Trial_name = "NCT00540514",
        Var1 = "Carboplatin+nab-Paclitaxel",
        Var2 = "Carboplatin+Paclitaxel",
        linenumber = 1,
        Var1_LoT = c(
            "Carboplatin,Paclitaxel Protein-Bound"
        ),
        Var2_LoT = c(
            "Carboplatin,Paclitaxel"
        ),
        histology = c("Non-squamous cell carcinoma", "Squamous cell carcinoma")
    ),
    ## Trial 11 - AURA3
    list(
        Trial_name = "AURA3",
        Var1 = "Osimertinib",
        Var2 = "Platinum+Pemetrexed",
        linenumber = 1,
        Var1_LoT = c(
            "Osimertinib"
        ),
        Var2_LoT = c(
            "Carboplatin,Pemetrexed",
            "Cisplatin,Pemetrexed"
        )
    ),
    ## Trial 12 - NCT00520676
    list(
        Trial_name = "NCT00520676",
        Var1 = "Carboplatin+Pemetrexed",
        Var2 = "Carboplatin+Docetaxel",
        linenumber = 1,
        Var1_LoT = c(
            "Carboplatin,Pemetrexed"
        ),
        Var2_LoT = c(
            "Carboplatin,Docetaxel"
        )
    )
)
