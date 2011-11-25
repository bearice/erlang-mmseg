#include<fcntl.h>
#include<stdio.h>
#include<stdlib.h>
#include<unistd.h>
#include<sys/socket.h>

#include<set>
#include<string>
#include<iostream>
#include<SegmenterManager.h>
#include<Segmenter.h>

using namespace std;

const char* skipWords_arr[] = {
    " ","\r","\n","\t",
    ",",".","!","@","#","$","%","^","&","*","(",")","-","=","_","+",
    "\"","'","~",";",":","<",">","/","?","[","]","{","}","\\","|",
    "！","￥","⋯","（","）","—","【","】","。","，","、","？","「","」",
    NULL,
};

uint32_t decodeUTF8(const char* cp){
    unsigned char c = *cp;
    if(c<0x80) return c;
    if(c<0xE0) return ((c & 0x1F) << 6) |  (cp[1] & 0x3F);
    if(c<0xF0) return ((c & 0x0F) <<12) | ((cp[1] & 0x3F) <<  6) |  (cp[2] & 0x3F);
    if(c<0xF8) return ((c & 0x07) <<18) | ((cp[1] & 0x3F) << 12) | ((cp[2] & 0x3F) <<  6) |  (cp[3] & 0x3F);
    if(c<0xFC) return ((c & 0x03) <<24) | ((cp[1] & 0x3F) << 18) | ((cp[2] & 0x3F) << 12) | ((cp[3] & 0x3F) <<  6) |  (cp[4] & 0x3F);
               return ((c & 0x01) <<30) | ((cp[1] & 0x3F) << 24) | ((cp[2] & 0x3F) << 18) | ((cp[3] & 0x3F) << 12) | ((cp[4] & 0x3F) << 6) | (cp[5] & 0x3F);
}

set<uint32_t> skipWords;
css::SegmenterManager g_mgr;

int init(const char *path){
	if(g_mgr.init(path) != 0) {
		perror(path);
		return 1;
	}
    const char** sw = skipWords_arr;
    while(*sw){
        skipWords.insert(decodeUTF8(*sw));
        //fprintf(stderr,"\r\n%s>0x%08x\r\n",*sw,getUTF8(*sw));    
        sw++;
    }
	return 0;
}

bool isSkipWord(const char* cp){
    //fprintf(stderr,"\r\n0x%08x\r\n",getUTF8(cp));    
    return skipWords.find(decodeUTF8(cp)) != skipWords.end();
}

int main(int argc,char** argv){
    if(init(argv[1] ? argv[1] : "/opt/local/etc/mmseg")){
        exit(1);   
    }

    uint16_t l;
    char     b[65536];
    while(1){
        if(!fread(&l,2,1,stdin)){
            break;   
        }
        l = ntohs(l);
        if(0==l){
            break;
        }
        if(!fread(b,l,1,stdin)){
            break;
        }
        css::Segmenter* seg = g_mgr.getSegmenter();
        seg->setBuffer((u1*)b,l);
        u2 len, symlen, nlen;
        while(1)
        { 
            len = symlen = 0;
            char* tok = (char*)seg->peekToken(len,symlen);
            if(!tok || !*tok || !len){
                break;
            }
            if(isSkipWord(tok)){
                seg->popToken(len);
                continue;
            }
            nlen = htons(len+2);
            fwrite(&nlen,2,1,stdout);
            nlen = htons(tok-b);
            fwrite(&nlen,2,1,stdout);
            fwrite(tok,len,1,stdout);
            seg->popToken(len);
/*            {
                const char* thesaurus_ptr = seg->thesaurus(tok, symlen);
                while(thesaurus_ptr && *thesaurus_ptr) {
                    len = strlen(thesaurus_ptr);
                    nlen = htons(len+2);
                    fwrite(&nlen,2,1,stdout);
                    nlen = htons(thesaurus_ptr-b);
                    fwrite(&nlen,2,1,stdout);
                    fwrite(thesaurus_ptr,len,1,stdout);
                    thesaurus_ptr += len + 1; //move next
                }
            }*/
        }
        nlen = htons(1);
        len  = 0;
        fwrite(&nlen,2,1,stdout);
        fwrite(&len,1,1,stdout);
        fflush(stdout);
        //fflush(stderr);
        g_mgr.clear();
    }
    return 0;
}
