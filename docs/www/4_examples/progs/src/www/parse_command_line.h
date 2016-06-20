/*
 * (C) Copyright 1996-2016 ECMWF.
 * 
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
 * In applying this licence, ECMWF does not waive the privileges and immunities 
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

C --------------------------------------------------------------------
C     PARSE_COMMAND_LINE
C     Checks the command-line for any arguments.
C     Arguments can come in pairs. Currently supported arguments are:
C     PROJECTION <CYLINDRICAL | POLAR_STEREOGRAPHIC>
C     DEVICE <PS | SVG | GD>
C     e.g. Run the program with:
C     <progname> PROJECTION POLAR_STEREOGRAPHIC
C     <progname> PROJECTION CYLINDRICAL   DEVICE SVG
C --------------------------------------------------------------------
C
      SUBROUTINE PARSE_COMMAND_LINE (OUTROOTNAME)
C
      CHARACTER*32 ARG
      CHARACTER*32 PROJECTION
      CHARACTER*32 DEVICE
      CHARACTER*64 OUTNAME
      CHARACTER*(*) OUTROOTNAME
      CHARACTER*16 FORMATS_PS_GIF
      DIMENSION    FORMATS_PS_GIF(2)
      DATA         FORMATS_PS_GIF /'PS', 'GIF_ANIMATION'/
C
      INTEGER NUM_ARGS
      INTEGER DEVICE_SET
      DEVICE_SET = 0
      NUM_ARGS = IARGC()
      I = 1
C
20    IF (I.LE.NUM_ARGS) THEN
          CALL GETARG ( I, ARG )         
C
C        Set the device?
C
          IF (ARG.EQ.'DEVICE') THEN
              I = I + 1 
              CALL GETARG ( I, DEVICE ) 
C
C             Set the output filename
C
              IF (DEVICE.EQ.'PS')  THEN
                OUTNAME = OUTROOTNAME //   '.ps'
                CALL PSETC ('DEVICE',       DEVICE)
                CALL PSETC ('PS_FILE_NAME', OUTNAME)
              ELSEIF (DEVICE.EQ.'PS_NEW') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'PS')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
              ELSEIF (DEVICE.EQ.'GIF') THEN
                CALL PSETC ('OUTPUT_FORMAT',  'GIF_ANIMATION')
                CALL PSETC ('OUTPUT_NAME', OUTROOTNAME)
                CALL PSETI ('OUTPUT_GIF_DELAY',     150)
C
              ELSEIF (DEVICE.EQ.'PS_GIF') THEN
                CALL PSET1C ('OUTPUT_FORMATS', FORMATS_PS_GIF, 2)
                CALL PSETC  ('OUTPUT_NAME', OUTROOTNAME)
              ELSE
                WRITE(*, '(A)') 'BAD DEVICE: ', DEVICE
              ENDIF
C            
            DEVICE_SET = 1
C
          ENDIF
C
          I = I + 1 
          GOTO 20
      ENDIF
C
C
C     If no device has been set, then use PostScript by default
C
      IF (DEVICE_SET.EQ.0) THEN
        OUTNAME = OUTROOTNAME // '.ps'
        CALL PSETC ('PS_FILE_NAME', OUTNAME)
      ENDIF
C
      END
