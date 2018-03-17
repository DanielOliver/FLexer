///////////////////////////////////////////////////////////////////////////////
// ADDINS
///////////////////////////////////////////////////////////////////////////////

#tool "nuget:?package=GitVersion.CommandLine&version=3.6.5"
#addin "nuget:?package=Cake.Figlet&version=1.0.0"
#addin nuget:?package=Cake.Npm
#addin nuget:?package=Cake.Netlify

///////////////////////////////////////////////////////////////////////////////
// ARGUMENTS
///////////////////////////////////////////////////////////////////////////////

var target = Argument("target", "Default");
var configuration = Argument("configuration", "Release");

///////////////////////////////////////////////////////////////////////////////
// CONFIGURATION
///////////////////////////////////////////////////////////////////////////////

var projectName = "FLexer";
var projectFile = File("./src/FLexer.Core/FLexer.Core.fsproj");
var playgroundProjectFile = File("./src/FLexer.Playground/FLexer.Playground.fsproj");
var testProjectFile = File("./src/FLexer.Tests/FLexer.Tests.fsproj");
var solutionFile = File("./src/FLexer.sln");
var nugetVersion = "";
var semVer = "";
var nugetApiKey = Argument("NUGET_API_KEY", EnvironmentVariable("NUGET_API_KEY"));
var netlifySiteID = Argument("NETLIFY_SITE_ID", EnvironmentVariable("NETLIFY_SITE_ID"));
var netlifyApiKey = Argument("NETLIFY_API_KEY", EnvironmentVariable("NETLIFY_API_KEY"));

var branch = EnvironmentVariable("APPVEYOR_REPO_BRANCH") ?? "";
var isMasterBranch = branch.ToUpper().Contains("MASTER");
var isTagged = (EnvironmentVariable("APPVEYOR_REPO_TAG") ?? "").Contains("true");
var tagName = EnvironmentVariable("APPVEYOR_REPO_TAG_NAME") ?? "";

var local = BuildSystem.IsLocalBuild;
var isRunningOnAppVeyor = AppVeyor.IsRunningOnAppVeyor;
var isPullRequest = AppVeyor.Environment.PullRequest.IsPullRequest;

///////////////////////////////////////////////////////////////////////////////
// SETUP / TEARDOWN
///////////////////////////////////////////////////////////////////////////////

Setup(ctx =>
{
   // Executed BEFORE the first task.
    Information(Figlet("FLexer"));
    Information("");
    Information("Branch: " + (branch ?? ""));
    Information("tagName: " + tagName);
    Information("IsTagged: " + (isTagged ? "true": "false"));
    Information("IsPullRequest: " + (isPullRequest ? "true": "false"));
});

Teardown(ctx =>
{
   // Executed AFTER the last task.
   Information("Finished running tasks.");
});

///////////////////////////////////////////////////////////////////////////////
// TASKS
///////////////////////////////////////////////////////////////////////////////

Task("clean")
    .Does(() =>
{
    CleanDirectories("./build");
    CleanDirectories("./node_modules");
    CleanDirectories("./**/bin");
    CleanDirectories("./**/obj");
    CreateDirectory("./build");
});

Task("get-version")
    .Does(() =>
{
    var version = GitVersion(new GitVersionSettings());

    semVer = version.SemVer;
    nugetVersion = version.NuGetVersionV2;

    Information("SemVer: " + semVer);
    Information("NuGet: " + nugetVersion);
});

Task("set-version")
    .Does(() =>
{
	var version = GitVersion(new GitVersionSettings {
        UpdateAssemblyInfo = true
    });

	semVer = version.SemVer;
	nugetVersion = version.NuGetVersionV2;

    // ReplaceProperty("Version", nugetVersion);
    // ReplaceProperty("Copyright", "Copyright (c) " + DateTime.Now.Year);

	Information("Nuget Version: " + nugetVersion);
    Information("SemVer: " + semVer);
});

Task("restore-nuget-packages")
    .IsDependentOn("clean")
    .Does(() =>
{
    Information("Restoring packages for {0}", solutionFile);

    DotNetCoreRestore(projectFile);
    DotNetCoreRestore(playgroundProjectFile);
});

Task("npm-install")
    .IsDependentOn("clean")
    .Does(() =>
{
    NpmInstall();
});

Task("test")
    .Does(() =>
{
    DotNetCoreTest(testProjectFile);
});


Task("build-fable")
    .IsDependentOn("restore-nuget-packages")
    .IsDependentOn("npm-install")
    .Does(() =>
{
    var exitCodeWithArgument =
        StartProcess(
            "dotnet",
            new ProcessSettings {
                Arguments = "fable npm-run build",
                WorkingDirectory = Directory("./src/FLexer.Playground")
            });
    // This should output 0 as valid arguments supplied
    if(exitCodeWithArgument == 0) {
        Information("Built Fable");
    } else {
        throw new System.Exception("Failed to build Fable");
    }
});

Task("pack")
    .IsDependentOn("set-version")
    .IsDependentOn("clean")
	.IsDependentOn("restore-nuget-packages")
    .IsDependentOn("test")
.Does(() =>
{
    var msBuildSettings = new DotNetCoreMSBuildSettings();
    msBuildSettings.Properties["PackageVersion"] = new[] { semVer };
    msBuildSettings.Properties["TargetFrameworks"] = new[] { "netstandard2.0" };

    var settings = new DotNetCorePackSettings
    {
        Configuration = "Release",
        OutputDirectory = "./build/",
        MSBuildSettings = msBuildSettings
    };

    DotNetCorePack("./src/FLexer.Core/FLexer.Core.fsproj", settings);
});

Task("push-nuget")
    .IsDependentOn("pack")
    .WithCriteria(() => isRunningOnAppVeyor && isMasterBranch && !isPullRequest && isTagged)
.Does(() =>
{
    var files = GetFiles("./build/*.nupkg");
    foreach(var nupkgFile in files)
    {
        Information("File: {0}", nupkgFile);
        var settings = new DotNetCoreNuGetPushSettings
        {
            ApiKey = nugetApiKey,
            Source = "https://api.nuget.org/v3/index.json"
        };

        DotNetCoreNuGetPush(nupkgFile.ToString(), settings);
    }
});

Task("push-netlify")
    .IsDependentOn("build-fable")
    .WithCriteria(() => isRunningOnAppVeyor && isMasterBranch && !isPullRequest)
.Does(() =>
{
    NetlifyDeploy("./public", netlifySiteID, netlifyApiKey);
});

Task("Default")
    .IsDependentOn("push-netlify")
    .IsDependentOn("push-nuget");

RunTarget(target);
