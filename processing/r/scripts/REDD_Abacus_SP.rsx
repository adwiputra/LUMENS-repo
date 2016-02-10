##[SCIENDO]=group
##Abacus_Project_File = file

if (file.exists("C:/Program Files (x86)/LUMENS/Abacus2")){
abacusExecutable = "C:/Progra~2/LUMENS/Abacus2/abacus2 "
} else{
abacusExecutable = "C:/Progra~1/LUMENS/Abacus2/abacus2 "
}

systemCommand <- paste(abacusExecutable, Abacus_Project_File)

system(systemCommand)
