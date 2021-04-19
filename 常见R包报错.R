1、问题一：library(org.Hs.eg.db)
出现如下报错：
Error: package or namespace load failed for ‘org.Hs.eg.db’:
 .onLoad failed in loadNamespace() for 'org.Hs.eg.db', details:
  call: l$contains
  error: $ operator is invalid for atomic vectors
解决：options(connectionObserver = NULL)
