# Revision history for lg

## 0.2.4 -- 2020-02-29
	* offsetToString This function transform the data type `FileOffset` in a string.
	* first_t This function extract the first element of a tuple (FileOffset, FilePath)
	* mix This function zip two lists, first element with the first, second with the second ...
	each pair of elements inside a list, the elements are strings.
	* twoInternalLst This function get a list of lists and return the head of sublist, a space and
	the tail of the sublist, recursively.

	This functions are necessary for transforming the data from [(FileOffset, FilePath)] to a list
	with sublist of strings for easy output with mapM_. This functions are pure.

## 0.2.3 -- 2020-02-26
	* getFileSize: More secure function, don't stop for broken symlinks, for default return size == 0.

## 0.2.2 -- 2020-02-25
	* Main: Makeup for main function.
	
## 0.2.1 -- 2020-02-25
	
	* First stable
	* mySort :: Ord b => [(a, b)] -> [(a, b)]
	Order only the type "b" of the tuple. FileOffset can be ordered, this type
	belong to the class Ord.
	* getFileSize :: FilePath -> IO FileOffset
	Return the filesize of the file (directory or file).
	* completePath :: FileSize -> IO [FilePath]
	Take a path directory and return the absolute path of the files (type directory and type
	file) in this directory.
	* getSize :: FilePath -> IO [FileOffset]
	Take a path of a file (dir or file), if this file is a directory return the size recursively.
	If is a normal file, return the size.
	* getSizeSum :: FilePath -> IO FileOffset
	Return the sum of the file (dir or file), because FileOffset is a type FileOffset = COff
	(synonymous) and this type belong of the clase Num and have a (+) function.
	* listGreater :: FilePath -> IO [(FilePath, FileOffset)]
	Get a filepath (dir or file) and return a list with the filename and the size, ordered
	from highest to lowest.
	
## 0.1.2 -- 2020-02-25

	* listGreater: Take a filepath, return the filename size (directory or file).
	
## 0.1.1 -- 2020-02-25

	* getFileSize: intuitively obvious.
	* completePath: Take a main directory and get the absolute path of the files.
	* getSize: Take a filepath (file or file directory), if is a directory return the size
	recursively. If is a file retur the size.
	* getSizeSum: Sum the filesizes of a file directory.
	* listGreater: Take a filepath, obtain the absolute path and return the filename and filesize.
	
