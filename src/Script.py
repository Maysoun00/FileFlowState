import re
import sys

# input-
# [1] - name of analysis
# [2] - name of info txt

graph = open(sys.argv[2], "r")
AnalyzerName = sys.argv[1]


################# Get info from the graph ####################

usings = []
states = []
analysisNames= []
analysisStatesNames =[]
analysisMethodsNames= []
analysisIDs = []
analysisTitles =[]
analysisErrorTypes = []
analysisMessages = []
analysisesBadStates = []
startState = ""
statesScope = "false"
edgesScope = "false"
funcs = []
analysisesMethodReports = []

for line in graph:
    if re.search("Analysis Name:", line):
        analysisNames.append(line.split(": ")[1].split("\n")[0])
    if re.search("ID:", line):
        analysisIDs.append(line.split(": ")[1].split("\n")[0])
    if re.search("Analysis message:", line):
        analysisMessages.append(line.split(": ")[1].split("\n")[0])
    if re.search("title:", line):
        analysisTitles.append(line.split(": ")[1].split("\n")[0])
    if re.search("type:", line):
        analysisErrorTypes.append(line.split(": ")[1].split("\n")[0])
    if re.search("ReportStates:", line):
        if len(line.split(": ")) != 1 :
            analysisesBadStates.append(line.split(": ")[1].split("\n")[0])
            analysisStatesNames.append(analysisNames[(len(analysisNames)-1)])
    if re.search("ReportsMethods:", line):
        if len(line.split(": ")) != 1 :
            analysisesMethodReports.append([line.split(": ")[1].split("\n")[0]])
            analysisMethodsNames.append(analysisNames[(len(analysisNames) - 1)])
    if re.search("}", line):
        statesScope = "false"
        edgesScope = "false"
    if re.search("using", line):
        usings.append(line.split("\n")[0])
    if statesScope  == "true" and line != "\n" :
        states.append(line.split(",")[0])
    if edgesScope == "true" and line != "\n":
        funcs.append([line.split(",")[1], line.split(",")[2].split("\n")[0]])
    if re.search(", S", line):
        if statesScope == "true":
            startState = line.split(",")[0]
    if re.search("States{", line):
        statesScope = "true"
    if re.search("Connections{", line):
        edgesScope = "true"
analysisesMethodReportsNew2 = []
for count, item in enumerate(analysisesBadStates):
    analysisesBadStates[count] = analysisesBadStates[count].split(", ")
for count, item in enumerate(analysisesMethodReports):
    analysisesMethodReportsNew = []
    for item2 in item[0].split(","):
        analysisesMethodReportsNew.append(item2)
    analysisesMethodReportsNew2.append(analysisesMethodReportsNew)
analysisesMethodReports = analysisesMethodReportsNew2
reportPerMethod = []
for state in states:
    reportPerMethod.append([state])
for count,reportMethod in enumerate(analysisesMethodReports):
    for method in reportMethod:
        state = method.split("-")[0].split("<")[1]
        for currstate in reportPerMethod:
            if currstate[0] == state:
                currstate.append([method.split("-")[1].split(">")[0],analysisMethodsNames[count]])


#################################################################################


########################## Input Checking #######################################

for bstate in analysisesBadStates:
    if(set(bstate).issubset(set(states))== False):
        print("There is incompatible state in bad states")
        exit()
if(len(analysisNames) != len(analysisMessages) != len(analysisIDs) != len(analysisTitles) != len(analysisErrorTypes)):
    print("Unmatched size of each params to each analysis (name, message, id, title, type)")
    exit()
if(startState == ""):
    print( "There isn't start state :(")
    exit()

#################################################################################

graph.close()

######################### Parsing State Enum File ###############################

file = open("TemplateState.cs", "r")
lines = file.readlines()
file.close()
output = open(AnalyzerName + "State.cs", "w")
for line in lines:
    if re.search("\\*\\*\\*PossibleStates\\*\\*\\*", line):
        for state in states:
            output.write("      " + state + ",\n")
    else:
        line = line.replace("***AnalyzerField***", AnalyzerName)
        output.write(line.replace("***FirstState***", startState))
output.close()

################################################################################

######################### Parsing Flow File ###############################

file = open("TemplateFlowState.cs", "r")
lines = file.readlines()
file.close()
output = open(AnalyzerName + "FlowState.cs", "w")
for line in lines:
    if re.search("\\*\\*\\*UsingOptions\\*\\*\\*", line):
        for using in usings:
            alreadyFound = "Notfound"
            for line2 in lines:
                if re.search(using, line2):
                    alreadyFound = "found"
            if (alreadyFound == "Notfound"):
                output.write(using)
    elif re.search("\\*\\*\\*OptionalInvocationMethods\\*\\*\\*", line):
        for func in funcs:
            namespace = ""
            for count, name in enumerate(func[0].split(".")):
                if(count != len(func[0].split("."))-2):
                    namespace = namespace + func[0].split(".")[count]+"."
                else:
                    namespace = namespace + func[0].split(".")[count]
                    break


            output.write("                       if (declaringTypeName == \"" + namespace+"\" &&")
            output.write(" ourMethodName.Identifier.ValueText.StartsWith(\""+ func[0].split(".")[len(func[0].split("."))-1] + "\"))\n")
            output.write("                       {\n")
            output.write("                             return " + AnalyzerName + "State." + func[1] + ";\n")
            output.write("                       }\n")


    else:
        line = line.replace("***AnalyzerField***", AnalyzerName)
        output.write(line.replace("***FirstState***", startState))
output.close()

#####################################################################################################

################################# Parsing Analyzer File #############################################

file = open("TemplateAnalyzer.cs", "r")
lines = file.readlines()
file.close()
output = open(AnalyzerName + "Analyzer.cs", "w")
for line in lines:
    if re.search("\\*\\*\\*UsingOptions\\*\\*\\*", line):
        for use in usings:
            alreadyFound = "Notfound"
            for line2 in lines:
                if re.search(use, line2):
                    alreadyFound = "found"
            if( alreadyFound == "Notfound"):
                output.write(use)
    elif re.search("\\*\\*\\*StringAnalysisID\\*\\*\\*",line):
        for count, item in enumerate(analysisIDs):
            output.write("      public const string " + analysisNames[count] + "Id = " + item + ";\n")
    elif re.search("\\*\\*\\*DiagnosticDescriptor\\*\\*\\*",line):
        for count, item in enumerate(analysisIDs):
            output.write("      internal static DiagnosticDescriptor " + analysisNames[count] + " =\n")
            output.write("          new DiagnosticDescriptor(\n")
            output.write("              id: " + analysisNames[count] + "Id,\n")
            output.write("              title: " + analysisTitles[count] + ",\n")
            output.write("              messageFormat: " + analysisMessages[count] + ",\n")
            output.write("              category: \"" + AnalyzerName + "\",\n")
            output.write("              defaultSeverity: DiagnosticSeverity." + analysisErrorTypes[count] + ",\n")
            output.write("              isEnabledByDefault: true);\n")
    elif re.search("\\*\\*\\*ReportingInvalidInvocation\\*\\*\\*",line):
        for count, diagnostic in enumerate(reportPerMethod):

            output.write("                      case " + AnalyzerName + "State." + diagnostic[0] + ":\n")
            for count,spesific in enumerate(diagnostic):
                if count == 0:
                    continue

                output.write("                           if (")
                if re.search("|", spesific[0]):

                    for count2, funcName in enumerate(spesific[0].split("|")):
                        if(count2 != (len(spesific[0].split("|"))-1)):
                            output.write("method.Name.StartsWith(\"" + funcName + "\") || ")
                        else:
                            output.write("method.Name.StartsWith(\"" + funcName + "\"))\n")
                else:
                    output.write("method.Name.StartsWith(\"" + spesific[0].split("-")[1].split(">")[0] + "\"))\n")
                output.write("                            {\n")
                output.write("                             context.ReportDiagnostic(Diagnostic.Create(" + spesific[1] + ", node.GetLocation()));\n")
                output.write("                            }\n")
            output.write("                            break;\n")
    elif re.search("\\*\\*\\*NamesOfAnalysis\\*\\*\\*",line):
        output.write(line.replace("***NamesOfAnalysis***", ', '.join(analysisNames)))
    elif re.search("\\*\\*\\*ReportingDiagnosticIfs\\*\\*\\*",line):
        for count, diagnostic in enumerate(analysisesBadStates):
            output.write(("                     if("))
            for countD, badState in enumerate(diagnostic):
                output.write("variableState.Value == " + AnalyzerName + "State." + badState + "")
                if( countD != len(diagnostic)-1):
                    output.write(" || ")
            output.write(")\n")
            output.write("                     {\n")
            output.write(("                          context.ReportDiagnostic(Diagnostic.Create(" + analysisStatesNames[count] + ", node.GetLocation()));\n"))
            output.write(("                     }\n"))



    else:
        line = line.replace("***AnalyzerField***", AnalyzerName)
        output.write(line.replace("***FirstState***", startState))
output.close()

###############################################################################################################################################
