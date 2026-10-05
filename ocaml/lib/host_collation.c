/* host_collation.c - ICU prefix semantics used by .NET's default StartsWith.
 * Collation-element algorithm adapted from dotnet/runtime pal_collation.c,
 * Copyright (c) .NET Foundation and Contributors. Licensed under the MIT license.
 * https://github.com/dotnet/runtime/blob/release/11.0/src/native/libs/System.Globalization.Native/pal_collation.c
 */
#define CAML_NAME_SPACE
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unicode/ucol.h>
#include <unicode/ucoleitr.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

/* .NET derives the current Unix culture from LC_ALL, LC_MESSAGES, then LANG.
 * C/POSIX and an unset locale correspond to the invariant root collator.
 */
static void culture_locale(char *buffer, size_t capacity) {
    const char *environment=getenv("LC_ALL");
    if (!environment || !*environment) environment=getenv("LC_MESSAGES");
    if (!environment || !*environment) environment=getenv("LANG");
    if (!environment) environment="";
    size_t length=strcspn(environment,".@");
    if (length>=capacity) length=capacity-1;
    memcpy(buffer,environment,length);buffer[length]='\0';
    if (!strcmp(buffer,"C") || !strcmp(buffer,"POSIX")) buffer[0]='\0';
}
static value current_culture_affix(value source_units,value pattern_units,int suffix) {
    CAMLparam2(source_units,pattern_units);
    mlsize_t source_length=Wosize_val(source_units),pattern_length=Wosize_val(pattern_units);
    if (pattern_length==0) CAMLreturn(Val_true);
    if (source_length>INT32_MAX || pattern_length>INT32_MAX) caml_failwith("ICU prefix text exceeds supported length");
    UChar *source=malloc((source_length ? source_length : 1)*sizeof(UChar));
    UChar *pattern=malloc(pattern_length*sizeof(UChar));
    if (!source || !pattern) { free(source);free(pattern);caml_raise_out_of_memory(); }
    for (mlsize_t i=0;i<source_length;i++) source[i]=(UChar)Long_val(Field(source_units,i));
    for (mlsize_t i=0;i<pattern_length;i++) pattern[i]=(UChar)Long_val(Field(pattern_units,i));
    char locale[256];culture_locale(locale,sizeof(locale));
    UErrorCode error=U_ZERO_ERROR;
    UCollator *collator=ucol_open(locale,&error);
    UCollationElements *pattern_iterator=NULL,*source_iterator=NULL;
    int result=0;
    if (U_SUCCESS(error)) pattern_iterator=ucol_openElements(collator,pattern,(int32_t)pattern_length,&error);
    if (U_SUCCESS(error)) source_iterator=ucol_openElements(collator,source,(int32_t)source_length,&error);
    if (U_SUCCESS(error)) {
        UCollationStrength strength=ucol_getStrength(collator);
        uint32_t mask=strength==UCOL_PRIMARY ? 0xffff0000u : strength==UCOL_SECONDARY ? 0xffffff00u : 0xffffffffu;
        int move_pattern=1,move_source=1;
        int32_t pattern_element=0,source_element=0;
        while (U_SUCCESS(error)) {
            if (move_pattern) pattern_element=suffix ? ucol_previous(pattern_iterator,&error) : ucol_next(pattern_iterator,&error);
            if (move_source) source_element=suffix ? ucol_previous(source_iterator,&error) : ucol_next(source_iterator,&error);
            move_pattern=1;move_source=1;
            if (pattern_element==UCOL_NULLORDER) {
                /* A following accent is part of the final source character;
                 * an ignorable following element does not extend that character.
                 */
                result=suffix || source_element==UCOL_NULLORDER || source_element==0 ||
                    (((uint32_t)source_element & 0xffff0000u)!=0 || ((uint32_t)source_element & 0x0000ff00u)==0);
                break;
            }
            if (pattern_element==0) move_source=0;
            else if (source_element==0) move_pattern=0;
            else if (((uint32_t)pattern_element & mask)!=((uint32_t)source_element & mask)) break;
        }
    }
    if (source_iterator) ucol_closeElements(source_iterator);
    if (pattern_iterator) ucol_closeElements(pattern_iterator);
    if (collator) ucol_close(collator);
    free(pattern);free(source);
    if (U_FAILURE(error)) caml_failwith("ICU prefix comparison failed");
    CAMLreturn(Val_bool(result));
}
CAMLprim value dark_starts_with_current_culture(value source_units,value pattern_units) {
    return current_culture_affix(source_units,pattern_units,0);
}
CAMLprim value dark_ends_with_current_culture(value source_units,value pattern_units) {
    return current_culture_affix(source_units,pattern_units,1);
}
