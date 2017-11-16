# A simple-to-use lexical analysis tool for students, researchers and interested persons to concurrently analyze and visualize the lexical similarity of multiple entities such as languages, implemented using R and Shiny. 

# Introduction
The goal of this project was to explore the similarity of different languages based on lexical distances and visualize the results in a meaningful way. We further developed a web application and deployed it using the web application framework "Shiny" by Rstudio. The web app is available under: http://kodu.ut.ee/~jaks/langdist 

# Preprocessing
To make the entities comparable, we got rid of irrelevant things such as punctuation, newline- and excessive whitespace characters. In addition we transformed all characters to lowercase since the distance measures are case sensitive.

We replaced all language-specific special characters with similar, more general characters. This process is known as transliteration (Latin to ASCII characters). For example, õ,ä,ö,ü are turned into o,a,o,u.

# Algorithm
For a more reliable algorithm, we combined both the normalized Damerau-Levenshtein distance and the length of the longest common subsequence to create an ensemble on a 40-60 basis, that is, 40% of the final distance score is attributed to the Damerau-Levenshtein measure and 60% is attributed to LCS.


# License
Copyright (c) 2015, Jakob Mass; Johann Lutterodt
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Jakob Mass; Johann Lutterodt nor the
   names of its contributors may be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Jakob Mass; Johann Lutterodt ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Jakob Mass; Johann Lutterodt BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
