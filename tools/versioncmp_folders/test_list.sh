#begin of modification - cgjd 26.07.2012
#new - search output images by name (ensure images are correctly compared)
# Utility to search an element in a list
search_list() {
echo "All Arguments to function: $*"
echo "First argument $1"
echo "Second argument $2"
echo "Third argument $3"
    elem= $1
    list= $2
    while [[ $iter -lt ${#list[*]} ]]; do
      if [[ elem = list[$iter] ]]; then return $iter; fi
      iter= $iter + 1
    done
    return -1
}
#end of modification - cgjd 26.07.2012

l[0]=uno
l[1]=dos
l[2]=tres

e=tres

res= search_list $e $l

echo $res, $l[$res]

