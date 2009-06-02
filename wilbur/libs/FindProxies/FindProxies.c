/*
 *  FindProxies.c
 *
 * --------------------------------------------------------------------------------------
 *
 *   The contents of this file are subject to the NOKOS License Version 1.0a (the
 *   "License"); you may not use this file except in compliance with the License. 
 *
 *   Software distributed under the License is distributed on an "AS IS" basis, WITHOUT
 *   WARRANTY OF ANY KIND, either express or implied. See the License for the specific
 *   language governing rights and limitations under the License. 
 *
 *   The Original Software is 
 *     WILBUR2: Nokia Semantic Web Toolkit for CLOS
 *
 *   Copyright (c) 2004 Nokia and others. All Rights Reserved.
 *   Portions Copyright (c) 1989-1992 Ora Lassila. All Rights Reserved.
 *
 *   Contributor(s): Ora Lassila (mailto:ora.lassila@nokia.com)
 *
 * --------------------------------------------------------------------------------------
 *
 *
 *   Purpose: Function to determine the current HTTP proxy on MacOS X
 *
 */

Boolean FindHTTPProxy(char *host, size_t hostSize)
{
  Boolean             result;
  CFDictionaryRef     proxyDict;
  CFNumberRef         enableNum;
  int                 enable;
  CFStringRef         hostStr;
  CFNumberRef         portNum;
  int                 portInt;
  
  result = false;
  proxyDict = SCDynamicStoreCopyProxies(NULL);
  if (proxyDict != NULL) {
    enableNum = (CFNumberRef)CFDictionaryGetValue(proxyDict, kSCPropNetProxiesHTTPEnable);
    if ((enableNum != NULL) &&
	    (CFGetTypeID(enableNum) == CFNumberGetTypeID()) &&
        CFNumberGetValue(enableNum, kCFNumberIntType, &enable) &&
		(enable != 0)) {
	  hostStr = (CFStringRef)CFDictionaryGetValue(proxyDict, kSCPropNetProxiesHTTPProxy);
	  if ((hostStr != NULL) &&
		  (CFGetTypeID(hostStr) == CFStringGetTypeID()) &&
		  CFStringGetCString(hostStr, host, (CFIndex)hostSize-7, kCFStringEncodingASCII)) {
		portNum = (CFNumberRef)CFDictionaryGetValue(proxyDict, kSCPropNetProxiesHTTPPort);
		if ((portNum != NULL) &&
		    (CFGetTypeID(portNum) == CFNumberGetTypeID()) &&
			CFNumberGetValue(portNum, kCFNumberIntType, &portInt)) {
          sprintf((char *)(host+strlen(host)), ":%u", (UInt16)portInt);
		  result = true;
		}
	  }
	}
  }
  if (proxyDict != NULL)
    CFRelease(proxyDict);
  if (!result)
    *host = 0;
  return result;
}
