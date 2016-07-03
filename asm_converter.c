#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include<stdint.h>  
#include<inttypes.h>
#include<endian.h> 

int main() {
    FILE * reader;
    char * line = NULL;
    size_t len = 0;
    ssize_t read;
    reader = fopen("sample_program.sasm", "r");
    int16_t codeArray[50000], codeArraySize = 0;
    while ((read = getline(&line, &len, reader)) != -1) {
		codeArray[codeArraySize++] = be16toh(atoi(line));
    }
    fclose(reader);
    FILE * writer;    
    writer = fopen("processor_emulator/prog.sextium", "w+");
    fwrite(codeArray, 2, codeArraySize + 1, writer);
    fclose(writer);   
    return(0);
}

