#!/bin/ksh

if [[ $# -eq 0 ]]; then
  echo "No folder specified. Exiting."
  exit 1
fi
# The path to the current script
SCRIPTDIR="${0%/*}"

# We define several warning codes
WARN_CODE_OKAY=0
WARN_CODE_DIFFERENT=1
WARN_CODE_CHECK=2
WARN_CODE_ERROR=3

# Each warning code has a different word associated with it
# which is used among other things as css tags on the html output
set -A warning_tags
warning_tags[$WARN_CODE_OKAY]="okay"
warning_tags[$WARN_CODE_DIFFERENT]="different"
warning_tags[$WARN_CODE_CHECK]="check"
warning_tags[$WARN_CODE_ERROR]="error"

# Get some essential associated information
output_root=$1
tools_home=$SCRIPTDIR/..
rundatetime=`date`


#begin of modification CGJD - 2012.08.06
#new - static
confluence_folder=$2
magpluslib=$3
#end of modification CGJD - 2012.08.06

#begin of modification CGJD - 2012.08.01
#new
main_head=$confluence_folder/main_head.html
main_body=$confluence_folder/main_body.html
main_tail=$confluence_folder/main_tail.html
rm -f $main_body
#end of modification CGJD - 2012.08.01

# Get a list of the tests that have been run from the set of 
# folders found in the output path
folders=`cd $output_root;find * -prune -type d`

# Work out where we will put our top level html file and
# copy all essential asociated files
toplevel_URL=$output_root/index.html
cp $tools_home/config/toplevel.html $toplevel_URL -f
cp $tools_home/config/codes.css $output_root

# we maintain many counters. This one is a count on the
# toplevel version we traverse (
toplevel_count=0

typeset -i zero
zero=0

toplevel_infostr=""

for folder in ${folders[*]}; do

  # take a copy of the blank set of test counters
  #toplevel_test_counters=$test_counters
  set -A toplevel_test_counters
  for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
     typeset -i zero
     toplevel_test_counters[$code]=$zero
   done

  versions=`cd $output_root/$folder; find * -prune -type d`


  if [[ toplevel_count -gt 0 ]]; then
    allversionstr+=", "
  fi
  allversionstr+="\"$folder\""

  versionstr=""
  count=0
  
  listing_URL=$output_root/$folder/index.html
  cp $tools_home/config/listing.html $listing_URL -f

  
    version_warningstr=""
    max_version_warning=0

    version_infostr=""

    for version in ${versions[*]}; do
    

        set -A version_test_counters
        for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
           typeset -i zero
           version_test_counters[$code]=$zero
         done

        if [[ count -gt 0 ]]; then
          versionstr+=", "
        fi
        versionstr+="\"$version\""

        overview_URL=$output_root/$folder/$version/index.html
        cp $tools_home/config/overview.html $overview_URL -f

        version1=$folder
        version2=$version

        tests=`cd $output_root/$folder/$version;find * -prune -type d`

        testcount=0
        rtncodestr=""
        warningstr=""
        extrainfostr=""

        max_test_warning=0

        test_warningstr=""

        for test in ${tests[*]}; do          

          if [[ $testcount -gt 0 ]]; then
            rtncodestr+=", "
            warningstr+=", "
            extrainfostr+=", "
          fi


          report=`cat $output_root/$folder/$version/$test/report.txt`      
          rtncodestr+="\"$test\":\"$report\""

          typeset -i warning_code
          warning_code=`cat $output_root/$folder/$version/$test/warning.txt`
          
          # store the maximum warning
          if [[ $warning_code -gt $max_test_warning ]]; then
            max_test_warning=$warning_code
          fi

          warningstr+="\"$test\":\"${warning_tags[$warning_code]}\""

          # dump the text that is in the summary for each test
          # so that it appears on the overview page
          if [[ $warning_code -eq $WARN_CODE_ERROR ]]; then
            extrainfo="Check the error output"
          else

            extrainfo=`sed "{:q;N;s/\n/\<BR\>/g;t q}" $output_root/$folder/$version/$test/summary.txt`
          fi

          extrainfostr+="\"$test\":\"$extrainfo\""


          (( version_test_counters[$warning_code]+=1 ))

          (( testcount += 1 ))
          (( topleveltestcount += 1))
        done
 

        if [[ $count -gt 0 ]]; then
          version_warningstr+=", "
        fi
        version_warningstr+="\"${warning_tags[$max_test_warning]}\""

        sed -i "s/%VERSION1%/$version1/g" $overview_URL
        sed -i "s/%VERSION2%/$version2/g" $overview_URL
        sed -i "s/%RUNDATETIME%/$rundatetime/g" $overview_URL
        sed -i "s/%RETURNSTRINGS%/$rtncodestr/g" $overview_URL
        sed -i "s/%WARNINGCODES%/$warningstr/g" $overview_URL
        sed -i "s/%EXTRAINFO%/$extrainfostr/g" $overview_URL


#begin of modification CGJD - 2012.03.29
#new
	# create an overview page for each type of warn code
	for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
	  codestr=${warning_tags[$code]}
	  overview_filter=$output_root/$folder/$version/$codestr.html
	  cp $tools_home/config/overview_filter.html $overview_filter -f
	  sed -i "s/%FILTER%/$codestr/g" $overview_filter
	  sed -i "s/%VERSION1%/$version1/g" $overview_filter
	  sed -i "s/%VERSION2%/$version2/g" $overview_filter
	  sed -i "s/%RUNDATETIME%/$rundatetime/g" $overview_filter
	  sed -i "s/%RETURNSTRINGS%/$rtncodestr/g" $overview_filter
	  sed -i "s/%WARNINGCODES%/$warningstr/g" $overview_filter
	  sed -i "s/%EXTRAINFO%/$extrainfostr/g" $overview_filter
	done
#end of modification CGJD - 2012.03.29



#begin of modification CGJD - 2012.08.01
#new
	echo $version1 | python -c "print raw_input().capitalize()" > ./version1.txt
        sed -i "s/+//g" ./version1.txt
	version1t=`cat ./version1.txt`
	rm -f ./version1.txt
	echo $version2 | python -c "print raw_input().capitalize()" > ./version2.txt
        sed -i "s/+//g" ./version2.txt
	version2t=`cat ./version2.txt`
	rm -f ./version2.txt
#end of modification CGJD - 2012.08.01


#begin of modification CGJD - 2012.04.16
#new
	# create an overview confluence page for each type of warn code
	for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
	  codestr=${warning_tags[$code]}
	  overview_filter=$confluence_folder/$version1t-$version2t:$codestr.html
	  cp $tools_home/config/confluence_overview_filter.html $overview_filter -f
	  sed -i "s/%MAGPLUSLIB%/$magpluslib/g" $overview_filter
	  sed -i "s/%FILTER%/$codestr/g" $overview_filter
	  sed -i "s/%VERSION1%/$version1/g" $overview_filter
	  sed -i "s/%VERSION2%/$version2/g" $overview_filter
	  sed -i "s/%RUNDATETIME%/$rundatetime/g" $overview_filter
	  sed -i "s/%RETURNSTRINGS%/$rtncodestr/g" $overview_filter
	  sed -i "s/%WARNINGCODES%/$warningstr/g" $overview_filter
	  sed -i "s/%EXTRAINFO%/$extrainfostr/g" $overview_filter
	done

	# create the overview confluence page for all warn codes
	overview_all=$confluence_folder/$version1t-$version2t.html
	cp $tools_home/config/confluence_overview.html $overview_all -f
	sed -i "s/%MAGPLUSLIB%/$magpluslib/g" $overview_all
	sed -i "s/%VERSION1%/$version1/g" $overview_all
	sed -i "s/%VERSION2%/$version2/g" $overview_all
	sed -i "s/%RUNDATETIME%/$rundatetime/g" $overview_all
	sed -i "s/%RETURNSTRINGS%/$rtncodestr/g" $overview_all
	sed -i "s/%WARNINGCODES%/$warningstr/g" $overview_all
	sed -i "s/%EXTRAINFO%/$extrainfostr/g" $overview_all
#end of modification CGJD - 2012.04.16

        # store the max warning found among compared versions
        if [[ $max_test_warning -gt $max_version_warning ]]; then
          max_version_warning=$max_test_warning
        fi

        (( count+=1 ))

        for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
           typeset -i sum
           sum=${toplevel_test_counters[$code]}+${version_test_counters[$code]}
           toplevel_test_counters[$code]=$sum
        done

        if [[ $version_infostr != "" ]]; then
          version_infostr+=", "
        fi
      
        version_infostr+="\"$version\": {"

        version_infostr_delta=""
        for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
          if [[ $version_infostr_delta != "" ]]; then
            version_infostr_delta+=", "
          fi
          codestr=${warning_tags[$code]}
          version_infostr_delta+="\"$codestr\": ${version_test_counters[$code]}"
        done
        version_infostr_delta+="}"

        version_infostr+=$version_infostr_delta
        

#begin of modification CGJD - 2012.04.16
#new
	# create the confluence listing page
	listing_con=$confluence_folder/listing.html
	cp $tools_home/config/confluence_main_body.html $listing_con -f
	sed -i "s/%MAGPLUSLIB%/$magpluslib/g" $listing_con
	sed -i "s/%VERSION1%/$version1/g" $listing_con
	sed -i "s/%VERSION2%/$version2/g" $listing_con
	sed -i "s/%VERSION1T%/$version1t/g" $listing_con
	sed -i "s/%VERSION2T%/$version2t/g" $listing_con
	sed -i "s/%RUNDATETIME%/$rundatetime/g" $listing_con
	sed -i "s/%NUM_OKAY%/${version_test_counters[$WARN_CODE_OKAY]}/g" $listing_con
	sed -i "s/%NUM_DIFFERENT%/${version_test_counters[$WARN_CODE_DIFFERENT]}/g" $listing_con
	sed -i "s/%NUM_CHECK%/${version_test_counters[$WARN_CODE_CHECK]}/g" $listing_con
	sed -i "s/%NUM_ERROR%/${version_test_counters[$WARN_CODE_ERROR]}/g" $listing_con
#end of modification CGJD - 2012.04.16

#begin of modification CGJD - 2012.08.01
#new
	cat $listing_con >> $main_body
	rm -f $listing_con
#end of modification CGJD - 2012.08.01


    done  # version loop

    sed -i "s/%INFOCODES%/$version_infostr/g" $listing_URL


    if [[ $toplevel_infostr != "" ]]; then
      toplevel_infostr+=", "
    fi

    toplevel_infostr_delta=""
    for code in $WARN_CODE_OKAY $WARN_CODE_DIFFERENT $WARN_CODE_CHECK $WARN_CODE_ERROR; do
      if [[ $toplevel_infostr_delta = "" ]]; then
        toplevel_infostr_delta="\"$folder\": {"
      else
        toplevel_infostr_delta+=", "
      fi
      codestr=${warning_tags[$code]}
      toplevel_infostr_delta+="\"$codestr\": ${toplevel_test_counters[$code]}"
    done
    toplevel_infostr_delta+="}"

    toplevel_infostr+=$toplevel_infostr_delta


  sed -i "s/%VERSION1%/$folder/g" $listing_URL
  sed -i "s/%VERSIONS%/$versionstr/g" $listing_URL
  sed -i "s/%RUNDATETIME%/$rundatetime/g" $listing_URL
  sed -i "s/%WARNINGCODES%/$version_warningstr/g" $listing_URL

  if [[ $toplevel_count -gt 0 ]]; then
    toplevel_warningstr+=", "
  fi
  toplevel_warningstr+="\"${warning_tags[$max_version_warning]}\""

  (( toplevel_count+=1 ))
done

#begin of modification CGJD - 2012.08.01
#new
  cp $tools_home/config/confluence_main_head.html $main_head -f
  sed -i "s/%VERSION1%/$version1/g" $main_head
  sed -i "s/%VERSION2%/$version2/g" $main_head
  sed -i "s/%RUNDATETIME%/$rundatetime/g" $main_head

  cp $tools_home/config/confluence_main_tail.html $main_tail -f

  cat $main_head $main_body $main_tail > $listing_con
  rm -f $main_head $main_body $main_tail
#end of modification CGJD - 2012.08.01

sed -i "s/%VERSION1%/Versions tested/g" $toplevel_URL
sed -i "s/%VERSIONS%/$allversionstr/g" $toplevel_URL
sed -i "s/%RUNDATETIME%/$rundatetime/g" $toplevel_URL
sed -i "s/%WARNINGCODES%/$toplevel_warningstr/g" $toplevel_URL
sed -i "s/%INFOCODES%/$toplevel_infostr/g" $toplevel_URL

#begin of modification CGJD - 2012.04.17
#new
# upload generated pages to confluence
#begin of modification CGJD - 2012.08.01
#old
#`$tools_home/confluence/add_pages.sh` 
#new
cd $confluence_folder
python add_pages.py > add_pages.out 2>&1
#end of modification CGJD - 2012.08.01
#end of modification CGJD - 2012.04.17
