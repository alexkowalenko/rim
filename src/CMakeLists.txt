#
# CMakeLists.txt libraries
#

#set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/src)

add_library(rimcommon STATIC
    parameters.f90
    adddat.f
    attadd.f attdel.f attget.f attnew.f attpag.f attput.f
    blkchg.f blkcln.f blkclr.f blkdef.f blkdsp.f blkdwn.f blkext.f
    blkloc.f blkmov.f blknxt.f blkup.f
    btadd.f btget.f btinit.f btlki.f btlkr.f btmove.f btput.f btrep.f btsert.f
    build.f 
    change.f chgdat.f chgpsw.f chkatt.f chkrel.f
    dbdefn.f dbload.f dbopen.f deldat.f 
    deldup.f delete.f delrow.f
    eq.f fmtdec.f
    getdat.f getkca.f gett.f gtsort.f
    isect.f
    join.f joirel.f kmpard.f kmpari.f kmparr.f kmpart.f kompxx.f
    locatt.f locboo.f
    locprm.f locrel.f
    lodele.f lodpas.f lodrel.f
    lstrel.f
    lxdate.f lxedit.f lxfmt.f
    macdef.f motscn.f ne.f 
    parse.f parval.f
    pject.f prckey.f prjtup.f
    putdat.f putt.f ptrs.f 
    reladd.f relget.f relpag.f relput.f remkey.f remlnk.f remrel.f rformt.f
    rmclos.f rmlook.f rmpii.f rmqery.f rmres.f rmsav.f rmvarc.f
    rnamea.f rnamel.f rnamer.f rtoa.f
    selrel.f selrpt.f selsrt.f selwhr.f
    sort.f subtrc.f swcon.f swcost.f
    swfilo.f swflfs.f 
    swhart.f swhrtd.f swhrti.f swhrtr.f 
    swicst.f swidcp.f swiicp.f swinpo.f swircp.f switcp.f 
    swshel.f swsink.f swsmfl.f swsmvl.f 
    swunlo.f swunvl.f 
    swvacp.f swvlfs.f swvllo.f 
    tokdsp.f tuplrc.f typer.f 
    union.f
    varadd.f 
    whetol.f wheval.f whrnam.f
    datetime.f90
    files.f90
    globals.f90
    macros.f90
    maths.f90
    message.f90
    lexer.f90
    parser.f90
    randomfiles.f90
    rim.f90
    system.f90
    text.f90
    textfiles.f90
    utils.f90
)

add_library(rimextlib STATIC
    amsg.f dmsg.f imsg.f
    lnkadd.f lnkget.f lnkpag.f lnkput.f loadfm.f
    loadit.f loadmv.f 
    loclnk.f locvar.f lodlnk.f  
    msg.f msgcmv.f 
    nxtcrd.f 
    pgbres.f pgbsto.f pgcexp.f
    pgcomp.f pgcprt.f pgeexp.f pgeprt.f pgexec.f pgstor.f
    prmset.f prompt.f
    selatt.f select.f selout.f selput.f 
    setin.f setout.f sgtlnk.f 
    undata.f undef.f unpass.f
    rim_cmd.f90
    system.f90
)

add_library(rimlib STATIC
    pilib.f pilibx.f
    rmxdel.f rmxget.f rmxlod.f rmxput.f
    system.f90
 )