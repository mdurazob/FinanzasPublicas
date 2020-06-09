import tabula

tabula.environment_info()


Ene2018 = "FP_201801.pdf"

TablaEne2018 = tabula.read_pdf(Ene2018, pages="4,7,55,56", stream=True)