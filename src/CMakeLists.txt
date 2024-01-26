#
# CMakeLists.txt libraries
#

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/src)

add_library(rimintlib STATIC
adddat.f amsg.f ascan.f asctxt.f ascdat.f
atoi.f ator.f attadd.f attdel.f attget.f
attnew.f attpag.f attput.f
blkchg.f blkcln.f blkclr.f blkdef.f blkdsp.f blkdwn.f
blkext.f blkloc.f blkmov.f blknxt.f blkup.f
btadd.f btget.f btinit.f btlki.f btlkr.f
btmove.f btput.f btrep.f btsert.f
build.f change.f chgdat.f chgpsw.f chkatt.f
chkrel.f chrasc.f chtype.f dbdefn.f
dbload.f dbopcl.f dbopen.f deldat.f deldup.f delete.f
delrow.f dmsg.f dtfenc.f dtfspl.f dtor.f eq.f eqkeyw.f eqtok.f
f1clo.f f1opn.f f2clo.f f2opn.f f3clo.f f3opn.f filch.f fmtdec.f
getdat.f getkca.f gett.f gtsort.f
iexp.f imsg.f isect.f itoa.f
join.f joirel.f kmpard.f kmpari.f kmparr.f kmpart.f kompxx.f
lfind.f lkstr.f lnkadd.f lnkget.f lnkpag.f lnkput.f loadfm.f
loadit.f loadmv.f locase.f locatt.f locboo.f loclnk.f locmac.f
locprm.f locrel.f locvar.f lodele.f lodlnk.f lodpas.f lodrec.f
lodrel.f lstrel.f lstrng.f lxcmnt.f lxdate.f lxedit.f lxfmt.f
lxinit.f lxlenw.f lxsrec.f lxwrec.f macdef.f macexp.f motscn.f
msg.f msgcmv.f ne.f nxtchr.f nxtchx.f
nxtcrd.f nxttok.f parse.f parval.f pgbres.f pgbsto.f pgcexp.f
pgcomp.f pgcprt.f pgeexp.f pgeprt.f pgexec.f pgstor.f
pject.f prckey.f prjtup.f prmset.f prompt.f ptrs.f putdat.f putt.f
reladd.f relget.f relpag.f relput.f remkey.f remlnk.f remrel.f
report.f reuse.f rformt.f rimcmd.f rioclo.f rioin.f rioopn.f
rioout.f ritoa.f rmclos.f rmcons.f rmhelp.f rminit.f
rmlook.f rmpii.f rmqery.f rmres.f rmsav.f rmset.f rmshow.f
rmtypt.f rmvarc.f rmzip.f rnamea.f rnamel.f rnamer.f
roun.f rtoa.f rtod.f
selatt.f select.f selout.f selput.f selrel.f selrpt.f selsrt.f
selwhr.f setin.f setout.f sgtlnk.f sort.f strasc.f strmov.f
subtrc.f swcon.f swcost.f swfilo.f swflfs.f swhart.f swhrtd.f swhrti.f
swhrtr.f swicst.f swidcp.f swiicp.f swinpo.f swircp.f switcp.f swshel.f
swsink.f swsmfl.f swsmvl.f swunlo.f swunvl.f swvacp.f swvlfs.f
swvllo.f tokdsp.f toktyp.f
toled.f toler.f tuplrc.f typer.f undata.f undef.f union.f
unload.f unpass.f varadd.f warn.f whetol.f
wheval.f whrnam.f xhibit.f
datetime.f90
system.f90
text.f90
textfiles.f90
utils.f90
)

add_library(rimlib STATIC
adddat.f ascan.f ascdat.f atoi.f ator.f
 attadd.f attdel.f attget.f attnew.f attpag.f attput.f
 blkchg.f blkcln.f blkclr.f blkdef.f blkdsp.f blkdwn.f blkext.f
 blkloc.f blkmov.f blknxt.f blkup.f btadd.f btget.f btinit.f
 btlki.f btlkr.f btmove.f btput.f btrep.f btsert.f build.f
 change.f chgdat.f chgpsw.f chkatt.f chkrel.f chrasc.f
 chtype.f dbdefn.f dbload.f dbopcl.f dbopen.f deldat.f
 deldup.f delete.f delrow.f dtfenc.f dtfspl.f dtor.f eq.f eqkeyw.f
 eqtok.f filch.f fmtdec.f f1clo.f f1opn.f f2clo.f f2opn.f
 f3clo.f f3opn.f getdat.f getkca.f gett.f gtsort.f
 iexp.f isect.f itoa.f join.f joirel.f
 kmpard.f kmpari.f kmparr.f kmpart.f kompxx.f lfind.f lkstr.f locase.f
 locatt.f locboo.f locmac.f locprm.f locrel.f lodele.f lodpas.f
 lodrec.f lodrel.f lstrel.f lstrng.f lxcmnt.f lxdate.f lxedit.f
 lxfmt.f lxinit.f lxlenw.f lxsrec.f lxwrec.f macdef.f macexp.f
 motscn.f ne.f nxtchr.f nxtchx.f nxttok.f
 parse.f parval.f pilib.f pilibx.f pject.f prckey.f prjtup.f
 ptrs.f putdat.f putt.f reladd.f relget.f relpag.f relput.f remkey.f
 remrel.f reuse.f rformt.f rioclo.f rioin.f rioopn.f rioout.f
 ritoa.f rmclos.f rmcons.f rminit.f rmlook.f rmpii.f
 rmqery.f rmres.f rmsav.f rmset.f rmtypt.f rmvarc.f
 rmxdel.f rmxget.f rmxlod.f rmxput.f rnamea.f rnamer.f roun.f
 rtoa.f rtod.f selrel.f selsrt.f selwhr.f sort.f strasc.f strmov.f 
 subtrc.f swcon.f swcost.f swfilo.f swflfs.f swhart.f swhrtd.f 
 swhrti.f swhrtr.f swicst.f swidcp.f swiicp.f swinpo.f swircp.f 
 switcp.f swshel.f swsink.f swsmfl.f swsmvl.f swunlo.f swunvl.f 
 swvacp.f swvlfs.f swvllo.f tokdsp.f toktyp.f toled.f toler.f tuplrc.f typer.f 
 union.f whetol.f wheval.f 
 whrnam.f xhibit.f
 datetime.f90
 text.f90
 utils.f90)

 add_library(system STATIC system.f90)
