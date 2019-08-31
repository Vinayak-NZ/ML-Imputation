## ---- Simulate-Missingness

# With respect to variable 'EconAct'
Census.EconAct.ohe <- SimMissV(dat=Census.test.ohe,
                   ident = 'person.id',
                   var.aux = c('region','residence.type','fam.comp','resident.type',
                               'sex','age','marital.status','birth.country',
                               'health','ethnicity','religion','occupation','industry'),
                   ohe.vars = c('region','resident.type','fam.comp','marital.status',
                                'health','ethnicity','religion','econ.act','occupation',
                                'industry','social.grade'),
                   imp.vars = c('econ.act','hours.cont','social.grade','student'),
                   var = 'econ.act',
                   ohe = 1,
                   exc = 1,
                   drop = 'econ.act.NCR')

Census.EconAct.label <- LabelMiss(
  dat.label = Census.test.label,
  dat.ohe = Census.EconAct.ohe,
  ident = 'person.id',
  ohe_vars = c('region','resident.type','fam.comp','marital.status',
               'health','ethnicity','religion','econ.act','occupation',
               'industry','social.grade'),
  keep_var = c('person.id', 'region', 'residence.type', 'fam.comp', 'resident.type',
               'sex', 'age', 'marital.status', 'student', 'birth.country', 'health',
               'ethnicity', 'religion', 'econ.act', 'occupation', 'industry', 'hours.cont',
               'social.grade'))

save(Census.EconAct.ohe, file = "data/ohe/missingness/Census.EconAct.ohe.RData")

save(Census.EconAct.label, file = "data/label/missingness/Census.EconAct.label.RData")


# With respect to variable 'HoursCont'
Census.HoursCont.ohe <- SimMissV(dat=Census.test.ohe,
                                ident = 'person.id',
                                var.aux = c('region','residence.type','fam.comp','resident.type',
                                            'sex','age','marital.status','birth.country',
                                            'health','ethnicity','religion','occupation','industry'),
                                ohe.vars = c('region','resident.type','fam.comp','marital.status',
                                             'health','ethnicity','religion','econ.act','occupation',
                                             'industry','social.grade'),
                                imp.vars = c('econ.act','hours.cont','social.grade','student'),
                                var = 'hours.cont',
                                ohe = 0,
                                exc = -9)

Census.HoursCont.label <- LabelMiss(
  dat.label = Census.test.label,
  dat.ohe = Census.HoursCont.ohe,
  ident = 'person.id',
  ohe_vars = c('region','resident.type','fam.comp','marital.status',
               'health','ethnicity','religion','econ.act','occupation',
               'industry','social.grade'),
  keep_var = c('person.id', 'region', 'residence.type', 'fam.comp', 'resident.type',
               'sex', 'age', 'marital.status', 'student', 'birth.country', 'health',
               'ethnicity', 'religion', 'econ.act', 'occupation', 'industry', 'hours.cont',
               'social.grade'))

save(Census.HoursCont.ohe, file = "data/ohe/missingness/Census.HoursCont.ohe.RData")

save(Census.HoursCont.label, file = "data/label/missingness/Census.HoursCont.label.RData")

# With respect to variable 'SocialGrade'
Census.SocialGrade.ohe <- SimMissV(dat=Census.test.ohe,
                                  ident = 'person.id',
                                  var.aux = c('region','residence.type','fam.comp','resident.type',
                                              'sex','age','marital.status','birth.country',
                                              'health','ethnicity','religion','occupation','industry'),
                                  ohe.vars = c('region','resident.type','fam.comp','marital.status',
                                               'health','ethnicity','religion','econ.act','occupation',
                                               'industry','social.grade'),
                                  imp.vars = c('econ.act','hours.cont','social.grade','student'),
                                  var = 'social.grade',
                                  ohe = 1,
                                  exc = 1,
                                  drop = 'social.grade.NCR')

Census.SocialGrade.label <- LabelMiss(
  dat.label = Census.test.label,
  dat.ohe = Census.SocialGrade.ohe,
  ident = 'person.id',
  ohe_vars = c('region','resident.type','fam.comp','marital.status',
               'health','ethnicity','religion','econ.act','occupation',
               'industry','social.grade'),
  keep_var = c('person.id', 'region', 'residence.type', 'fam.comp', 'resident.type',
               'sex', 'age', 'marital.status', 'student', 'birth.country', 'health',
               'ethnicity', 'religion', 'econ.act', 'occupation', 'industry', 'hours.cont',
               'social.grade'))

save(Census.SocialGrade.ohe, file = "data/ohe/missingness/Census.SocialGrade.ohe.RData")

save(Census.SocialGrade.label, file = "data/label/missingness/Census.SocialGrade.label.RData")

# With respect to variable 'Student'
Census.Student.ohe <- SimMissV(dat=Census.test.ohe,
                                  ident = 'person.id',
                                  var.aux = c('region','residence.type','fam.comp','resident.type',
                                              'sex','age','marital.status','birth.country',
                                              'health','ethnicity','religion','occupation','industry'),
                                  ohe.vars = c('region','resident.type','fam.comp','marital.status',
                                               'health','ethnicity','religion','econ.act','occupation',
                                               'industry','social.grade'),
                                  imp.vars = c('econ.act','hours.cont','social.grade','student'),
                                  var = 'student',
                                  ohe = 0,
                                  exc = -9)

Census.Student.label <- LabelMiss(
  dat.label = Census.test.label,
  dat.ohe = Census.Student.ohe,
  ident = 'person.id',
  ohe_vars = c('region','resident.type','fam.comp','marital.status',
               'health','ethnicity','religion','econ.act','occupation',
               'industry','social.grade'),
  keep_var = c('person.id', 'region', 'residence.type', 'fam.comp', 'resident.type',
               'sex', 'age', 'marital.status', 'student', 'birth.country', 'health',
               'ethnicity', 'religion', 'econ.act', 'occupation', 'industry', 'hours.cont',
               'social.grade'))

save(Census.Student.ohe, file = "data/ohe/missingness/Census.Student.ohe.RData")

save(Census.Student.label, file = "data/label/missingness/Census.Student.label.RData")
