
# -------------------------------------------------------------------
# function msg(strMessage)
#
# Iain Russell, August 2005
#
# Echos the given argument to standard output.
# If the environment variable $SMSNAME is defined, then the 
# environment variable $SMSLABEL is queried and the message is also
# sent to that SMS label.
# -------------------------------------------------------------------

msg(){

    # retrieve the message from the argument list

    if [[ "$1" = "" ]]
        then strMessage="No Message"
        else strMessage=$1
    fi


    # echo to standard output

    echo $strMessage


    # test for SMS environment

    if [[ "$SMSNAME" != "" ]] 
    then   
        if [[ "$SMSLABEL" != "" ]] 
        then   
            smslabel $SMSLABEL $strMessage
        fi
    fi 

}
