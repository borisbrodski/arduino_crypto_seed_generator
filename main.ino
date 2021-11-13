#include <LiquidCrystal.h>
#include <Encoder.h>

#define SHA256_DISABLE_WRAPPER
#include <sha256.h>

#define sign(x) ((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))

#define BUTTON_DEBOUNCE_DELAY     20    // [ms]

#define LCD_RS 12
#define LCD_EN 11
#define LCD_D4 5
#define LCD_D5 4
#define LCD_D6 3
#define LCD_D7 2


#define ROTARY_ENCODER_PIN1 18
#define ROTARY_ENCODER_PIN2 19
#define BUTTON_OK_PIN   6


#define WORDLIST_MAX_WORD_LENGTH  8
#define WORDLIST_CHUNK_MAX_LENGTH 122

const char wordlist_chunk_0[] PROGMEM   = "abandon,ability,able,about,above,absent,absorb,abstract,absurd,abuse,access,accident,account,accuse,achieve,acid";
const char wordlist_chunk_1[] PROGMEM   = "acoustic,acquire,across,act,action,actor,actress,actual,adapt,add,addict,address,adjust,admit,adult,advance";
const char wordlist_chunk_2[] PROGMEM   = "advice,aerobic,affair,afford,afraid,again,age,agent,agree,ahead,aim,air,airport,aisle,alarm,album";
const char wordlist_chunk_3[] PROGMEM   = "alcohol,alert,alien,all,alley,allow,almost,alone,alpha,already,also,alter,always,amateur,amazing,among";
const char wordlist_chunk_4[] PROGMEM   = "amount,amused,analyst,anchor,ancient,anger,angle,angry,animal,ankle,announce,annual,another,answer,antenna,antique";
const char wordlist_chunk_5[] PROGMEM   = "anxiety,any,apart,apology,appear,apple,approve,april,arch,arctic,area,arena,argue,arm,armed,armor";
const char wordlist_chunk_6[] PROGMEM   = "army,around,arrange,arrest,arrive,arrow,art,artefact,artist,artwork,ask,aspect,assault,asset,assist,assume";
const char wordlist_chunk_7[] PROGMEM   = "asthma,athlete,atom,attack,attend,attitude,attract,auction,audit,august,aunt,author,auto,autumn,average,avocado";
const char wordlist_chunk_8[] PROGMEM   = "avoid,awake,aware,away,awesome,awful,awkward,axis,baby,bachelor,bacon,badge,bag,balance,balcony,ball";
const char wordlist_chunk_9[] PROGMEM   = "bamboo,banana,banner,bar,barely,bargain,barrel,base,basic,basket,battle,beach,bean,beauty,because,become";
const char wordlist_chunk_10[] PROGMEM  = "beef,before,begin,behave,behind,believe,below,belt,bench,benefit,best,betray,better,between,beyond,bicycle";
const char wordlist_chunk_11[] PROGMEM  = "bid,bike,bind,biology,bird,birth,bitter,black,blade,blame,blanket,blast,bleak,bless,blind,blood";
const char wordlist_chunk_12[] PROGMEM  = "blossom,blouse,blue,blur,blush,board,boat,body,boil,bomb,bone,bonus,book,boost,border,boring";
const char wordlist_chunk_13[] PROGMEM  = "borrow,boss,bottom,bounce,box,boy,bracket,brain,brand,brass,brave,bread,breeze,brick,bridge,brief";
const char wordlist_chunk_14[] PROGMEM  = "bright,bring,brisk,broccoli,broken,bronze,broom,brother,brown,brush,bubble,buddy,budget,buffalo,build,bulb";
const char wordlist_chunk_15[] PROGMEM  = "bulk,bullet,bundle,bunker,burden,burger,burst,bus,business,busy,butter,buyer,buzz,cabbage,cabin,cable";
const char wordlist_chunk_16[] PROGMEM  = "cactus,cage,cake,call,calm,camera,camp,can,canal,cancel,candy,cannon,canoe,canvas,canyon,capable";
const char wordlist_chunk_17[] PROGMEM  = "capital,captain,car,carbon,card,cargo,carpet,carry,cart,case,cash,casino,castle,casual,cat,catalog";
const char wordlist_chunk_18[] PROGMEM  = "catch,category,cattle,caught,cause,caution,cave,ceiling,celery,cement,census,century,cereal,certain,chair,chalk";
const char wordlist_chunk_19[] PROGMEM  = "champion,change,chaos,chapter,charge,chase,chat,cheap,check,cheese,chef,cherry,chest,chicken,chief,child";
const char wordlist_chunk_20[] PROGMEM  = "chimney,choice,choose,chronic,chuckle,chunk,churn,cigar,cinnamon,circle,citizen,city,civil,claim,clap,clarify";
const char wordlist_chunk_21[] PROGMEM  = "claw,clay,clean,clerk,clever,click,client,cliff,climb,clinic,clip,clock,clog,close,cloth,cloud";
const char wordlist_chunk_22[] PROGMEM  = "clown,club,clump,cluster,clutch,coach,coast,coconut,code,coffee,coil,coin,collect,color,column,combine";
const char wordlist_chunk_23[] PROGMEM  = "come,comfort,comic,common,company,concert,conduct,confirm,congress,connect,consider,control,convince,cook,cool,copper";
const char wordlist_chunk_24[] PROGMEM  = "copy,coral,core,corn,correct,cost,cotton,couch,country,couple,course,cousin,cover,coyote,crack,cradle";
const char wordlist_chunk_25[] PROGMEM  = "craft,cram,crane,crash,crater,crawl,crazy,cream,credit,creek,crew,cricket,crime,crisp,critic,crop";
const char wordlist_chunk_26[] PROGMEM  = "cross,crouch,crowd,crucial,cruel,cruise,crumble,crunch,crush,cry,crystal,cube,culture,cup,cupboard,curious";
const char wordlist_chunk_27[] PROGMEM  = "current,curtain,curve,cushion,custom,cute,cycle,dad,damage,damp,dance,danger,daring,dash,daughter,dawn";
const char wordlist_chunk_28[] PROGMEM  = "day,deal,debate,debris,decade,december,decide,decline,decorate,decrease,deer,defense,define,defy,degree,delay";
const char wordlist_chunk_29[] PROGMEM  = "deliver,demand,demise,denial,dentist,deny,depart,depend,deposit,depth,deputy,derive,describe,desert,design,desk";
const char wordlist_chunk_30[] PROGMEM  = "despair,destroy,detail,detect,develop,device,devote,diagram,dial,diamond,diary,dice,diesel,diet,differ,digital";
const char wordlist_chunk_31[] PROGMEM  = "dignity,dilemma,dinner,dinosaur,direct,dirt,disagree,discover,disease,dish,dismiss,disorder,display,distance,divert,divide";
const char wordlist_chunk_32[] PROGMEM  = "divorce,dizzy,doctor,document,dog,doll,dolphin,domain,donate,donkey,donor,door,dose,double,dove,draft";
const char wordlist_chunk_33[] PROGMEM  = "dragon,drama,drastic,draw,dream,dress,drift,drill,drink,drip,drive,drop,drum,dry,duck,dumb";
const char wordlist_chunk_34[] PROGMEM  = "dune,during,dust,dutch,duty,dwarf,dynamic,eager,eagle,early,earn,earth,easily,east,easy,echo";
const char wordlist_chunk_35[] PROGMEM  = "ecology,economy,edge,edit,educate,effort,egg,eight,either,elbow,elder,electric,elegant,element,elephant,elevator";
const char wordlist_chunk_36[] PROGMEM  = "elite,else,embark,embody,embrace,emerge,emotion,employ,empower,empty,enable,enact,end,endless,endorse,enemy";
const char wordlist_chunk_37[] PROGMEM  = "energy,enforce,engage,engine,enhance,enjoy,enlist,enough,enrich,enroll,ensure,enter,entire,entry,envelope,episode";
const char wordlist_chunk_38[] PROGMEM  = "equal,equip,era,erase,erode,erosion,error,erupt,escape,essay,essence,estate,eternal,ethics,evidence,evil";
const char wordlist_chunk_39[] PROGMEM  = "evoke,evolve,exact,example,excess,exchange,excite,exclude,excuse,execute,exercise,exhaust,exhibit,exile,exist,exit";
const char wordlist_chunk_40[] PROGMEM  = "exotic,expand,expect,expire,explain,expose,express,extend,extra,eye,eyebrow,fabric,face,faculty,fade,faint";
const char wordlist_chunk_41[] PROGMEM  = "faith,fall,false,fame,family,famous,fan,fancy,fantasy,farm,fashion,fat,fatal,father,fatigue,fault";
const char wordlist_chunk_42[] PROGMEM  = "favorite,feature,february,federal,fee,feed,feel,female,fence,festival,fetch,fever,few,fiber,fiction,field";
const char wordlist_chunk_43[] PROGMEM  = "figure,file,film,filter,final,find,fine,finger,finish,fire,firm,first,fiscal,fish,fit,fitness";
const char wordlist_chunk_44[] PROGMEM  = "fix,flag,flame,flash,flat,flavor,flee,flight,flip,float,flock,floor,flower,fluid,flush,fly";
const char wordlist_chunk_45[] PROGMEM  = "foam,focus,fog,foil,fold,follow,food,foot,force,forest,forget,fork,fortune,forum,forward,fossil";
const char wordlist_chunk_46[] PROGMEM  = "foster,found,fox,fragile,frame,frequent,fresh,friend,fringe,frog,front,frost,frown,frozen,fruit,fuel";
const char wordlist_chunk_47[] PROGMEM  = "fun,funny,furnace,fury,future,gadget,gain,galaxy,gallery,game,gap,garage,garbage,garden,garlic,garment";
const char wordlist_chunk_48[] PROGMEM  = "gas,gasp,gate,gather,gauge,gaze,general,genius,genre,gentle,genuine,gesture,ghost,giant,gift,giggle";
const char wordlist_chunk_49[] PROGMEM  = "ginger,giraffe,girl,give,glad,glance,glare,glass,glide,glimpse,globe,gloom,glory,glove,glow,glue";
const char wordlist_chunk_50[] PROGMEM  = "goat,goddess,gold,good,goose,gorilla,gospel,gossip,govern,gown,grab,grace,grain,grant,grape,grass";
const char wordlist_chunk_51[] PROGMEM  = "gravity,great,green,grid,grief,grit,grocery,group,grow,grunt,guard,guess,guide,guilt,guitar,gun";
const char wordlist_chunk_52[] PROGMEM  = "gym,habit,hair,half,hammer,hamster,hand,happy,harbor,hard,harsh,harvest,hat,have,hawk,hazard";
const char wordlist_chunk_53[] PROGMEM  = "head,health,heart,heavy,hedgehog,height,hello,helmet,help,hen,hero,hidden,high,hill,hint,hip";
const char wordlist_chunk_54[] PROGMEM  = "hire,history,hobby,hockey,hold,hole,holiday,hollow,home,honey,hood,hope,horn,horror,horse,hospital";
const char wordlist_chunk_55[] PROGMEM  = "host,hotel,hour,hover,hub,huge,human,humble,humor,hundred,hungry,hunt,hurdle,hurry,hurt,husband";
const char wordlist_chunk_56[] PROGMEM  = "hybrid,ice,icon,idea,identify,idle,ignore,ill,illegal,illness,image,imitate,immense,immune,impact,impose";
const char wordlist_chunk_57[] PROGMEM  = "improve,impulse,inch,include,income,increase,index,indicate,indoor,industry,infant,inflict,inform,inhale,inherit,initial";
const char wordlist_chunk_58[] PROGMEM  = "inject,injury,inmate,inner,innocent,input,inquiry,insane,insect,inside,inspire,install,intact,interest,into,invest";
const char wordlist_chunk_59[] PROGMEM  = "invite,involve,iron,island,isolate,issue,item,ivory,jacket,jaguar,jar,jazz,jealous,jeans,jelly,jewel";
const char wordlist_chunk_60[] PROGMEM  = "job,join,joke,journey,joy,judge,juice,jump,jungle,junior,junk,just,kangaroo,keen,keep,ketchup";
const char wordlist_chunk_61[] PROGMEM  = "key,kick,kid,kidney,kind,kingdom,kiss,kit,kitchen,kite,kitten,kiwi,knee,knife,knock,know";
const char wordlist_chunk_62[] PROGMEM  = "lab,label,labor,ladder,lady,lake,lamp,language,laptop,large,later,latin,laugh,laundry,lava,law";
const char wordlist_chunk_63[] PROGMEM  = "lawn,lawsuit,layer,lazy,leader,leaf,learn,leave,lecture,left,leg,legal,legend,leisure,lemon,lend";
const char wordlist_chunk_64[] PROGMEM  = "length,lens,leopard,lesson,letter,level,liar,liberty,library,license,life,lift,light,like,limb,limit";
const char wordlist_chunk_65[] PROGMEM  = "link,lion,liquid,list,little,live,lizard,load,loan,lobster,local,lock,logic,lonely,long,loop";
const char wordlist_chunk_66[] PROGMEM  = "lottery,loud,lounge,love,loyal,lucky,luggage,lumber,lunar,lunch,luxury,lyrics,machine,mad,magic,magnet";
const char wordlist_chunk_67[] PROGMEM  = "maid,mail,main,major,make,mammal,man,manage,mandate,mango,mansion,manual,maple,marble,march,margin";
const char wordlist_chunk_68[] PROGMEM  = "marine,market,marriage,mask,mass,master,match,material,math,matrix,matter,maximum,maze,meadow,mean,measure";
const char wordlist_chunk_69[] PROGMEM  = "meat,mechanic,medal,media,melody,melt,member,memory,mention,menu,mercy,merge,merit,merry,mesh,message";
const char wordlist_chunk_70[] PROGMEM  = "metal,method,middle,midnight,milk,million,mimic,mind,minimum,minor,minute,miracle,mirror,misery,miss,mistake";
const char wordlist_chunk_71[] PROGMEM  = "mix,mixed,mixture,mobile,model,modify,mom,moment,monitor,monkey,monster,month,moon,moral,more,morning";
const char wordlist_chunk_72[] PROGMEM  = "mosquito,mother,motion,motor,mountain,mouse,move,movie,much,muffin,mule,multiply,muscle,museum,mushroom,music";
const char wordlist_chunk_73[] PROGMEM  = "must,mutual,myself,mystery,myth,naive,name,napkin,narrow,nasty,nation,nature,near,neck,need,negative";
const char wordlist_chunk_74[] PROGMEM  = "neglect,neither,nephew,nerve,nest,net,network,neutral,never,news,next,nice,night,noble,noise,nominee";
const char wordlist_chunk_75[] PROGMEM  = "noodle,normal,north,nose,notable,note,nothing,notice,novel,now,nuclear,number,nurse,nut,oak,obey";
const char wordlist_chunk_76[] PROGMEM  = "object,oblige,obscure,observe,obtain,obvious,occur,ocean,october,odor,off,offer,office,often,oil,okay";
const char wordlist_chunk_77[] PROGMEM  = "old,olive,olympic,omit,once,one,onion,online,only,open,opera,opinion,oppose,option,orange,orbit";
const char wordlist_chunk_78[] PROGMEM  = "orchard,order,ordinary,organ,orient,original,orphan,ostrich,other,outdoor,outer,output,outside,oval,oven,over";
const char wordlist_chunk_79[] PROGMEM  = "own,owner,oxygen,oyster,ozone,pact,paddle,page,pair,palace,palm,panda,panel,panic,panther,paper";
const char wordlist_chunk_80[] PROGMEM  = "parade,parent,park,parrot,party,pass,patch,path,patient,patrol,pattern,pause,pave,payment,peace,peanut";
const char wordlist_chunk_81[] PROGMEM  = "pear,peasant,pelican,pen,penalty,pencil,people,pepper,perfect,permit,person,pet,phone,photo,phrase,physical";
const char wordlist_chunk_82[] PROGMEM  = "piano,picnic,picture,piece,pig,pigeon,pill,pilot,pink,pioneer,pipe,pistol,pitch,pizza,place,planet";
const char wordlist_chunk_83[] PROGMEM  = "plastic,plate,play,please,pledge,pluck,plug,plunge,poem,poet,point,polar,pole,police,pond,pony";
const char wordlist_chunk_84[] PROGMEM  = "pool,popular,portion,position,possible,post,potato,pottery,poverty,powder,power,practice,praise,predict,prefer,prepare";
const char wordlist_chunk_85[] PROGMEM  = "present,pretty,prevent,price,pride,primary,print,priority,prison,private,prize,problem,process,produce,profit,program";
const char wordlist_chunk_86[] PROGMEM  = "project,promote,proof,property,prosper,protect,proud,provide,public,pudding,pull,pulp,pulse,pumpkin,punch,pupil";
const char wordlist_chunk_87[] PROGMEM  = "puppy,purchase,purity,purpose,purse,push,put,puzzle,pyramid,quality,quantum,quarter,question,quick,quit,quiz";
const char wordlist_chunk_88[] PROGMEM  = "quote,rabbit,raccoon,race,rack,radar,radio,rail,rain,raise,rally,ramp,ranch,random,range,rapid";
const char wordlist_chunk_89[] PROGMEM  = "rare,rate,rather,raven,raw,razor,ready,real,reason,rebel,rebuild,recall,receive,recipe,record,recycle";
const char wordlist_chunk_90[] PROGMEM  = "reduce,reflect,reform,refuse,region,regret,regular,reject,relax,release,relief,rely,remain,remember,remind,remove";
const char wordlist_chunk_91[] PROGMEM  = "render,renew,rent,reopen,repair,repeat,replace,report,require,rescue,resemble,resist,resource,response,result,retire";
const char wordlist_chunk_92[] PROGMEM  = "retreat,return,reunion,reveal,review,reward,rhythm,rib,ribbon,rice,rich,ride,ridge,rifle,right,rigid";
const char wordlist_chunk_93[] PROGMEM  = "ring,riot,ripple,risk,ritual,rival,river,road,roast,robot,robust,rocket,romance,roof,rookie,room";
const char wordlist_chunk_94[] PROGMEM  = "rose,rotate,rough,round,route,royal,rubber,rude,rug,rule,run,runway,rural,sad,saddle,sadness";
const char wordlist_chunk_95[] PROGMEM  = "safe,sail,salad,salmon,salon,salt,salute,same,sample,sand,satisfy,satoshi,sauce,sausage,save,say";
const char wordlist_chunk_96[] PROGMEM  = "scale,scan,scare,scatter,scene,scheme,school,science,scissors,scorpion,scout,scrap,screen,script,scrub,sea";
const char wordlist_chunk_97[] PROGMEM  = "search,season,seat,second,secret,section,security,seed,seek,segment,select,sell,seminar,senior,sense,sentence";
const char wordlist_chunk_98[] PROGMEM  = "series,service,session,settle,setup,seven,shadow,shaft,shallow,share,shed,shell,sheriff,shield,shift,shine";
const char wordlist_chunk_99[] PROGMEM  = "ship,shiver,shock,shoe,shoot,shop,short,shoulder,shove,shrimp,shrug,shuffle,shy,sibling,sick,side";
const char wordlist_chunk_100[] PROGMEM = "siege,sight,sign,silent,silk,silly,silver,similar,simple,since,sing,siren,sister,situate,six,size";
const char wordlist_chunk_101[] PROGMEM = "skate,sketch,ski,skill,skin,skirt,skull,slab,slam,sleep,slender,slice,slide,slight,slim,slogan";
const char wordlist_chunk_102[] PROGMEM = "slot,slow,slush,small,smart,smile,smoke,smooth,snack,snake,snap,sniff,snow,soap,soccer,social";
const char wordlist_chunk_103[] PROGMEM = "sock,soda,soft,solar,soldier,solid,solution,solve,someone,song,soon,sorry,sort,soul,sound,soup";
const char wordlist_chunk_104[] PROGMEM = "source,south,space,spare,spatial,spawn,speak,special,speed,spell,spend,sphere,spice,spider,spike,spin";
const char wordlist_chunk_105[] PROGMEM = "spirit,split,spoil,sponsor,spoon,sport,spot,spray,spread,spring,spy,square,squeeze,squirrel,stable,stadium";
const char wordlist_chunk_106[] PROGMEM = "staff,stage,stairs,stamp,stand,start,state,stay,steak,steel,stem,step,stereo,stick,still,sting";
const char wordlist_chunk_107[] PROGMEM = "stock,stomach,stone,stool,story,stove,strategy,street,strike,strong,struggle,student,stuff,stumble,style,subject";
const char wordlist_chunk_108[] PROGMEM = "submit,subway,success,such,sudden,suffer,sugar,suggest,suit,summer,sun,sunny,sunset,super,supply,supreme";
const char wordlist_chunk_109[] PROGMEM = "sure,surface,surge,surprise,surround,survey,suspect,sustain,swallow,swamp,swap,swarm,swear,sweet,swift,swim";
const char wordlist_chunk_110[] PROGMEM = "swing,switch,sword,symbol,symptom,syrup,system,table,tackle,tag,tail,talent,talk,tank,tape,target";
const char wordlist_chunk_111[] PROGMEM = "task,taste,tattoo,taxi,teach,team,tell,ten,tenant,tennis,tent,term,test,text,thank,that";
const char wordlist_chunk_112[] PROGMEM = "theme,then,theory,there,they,thing,this,thought,three,thrive,throw,thumb,thunder,ticket,tide,tiger";
const char wordlist_chunk_113[] PROGMEM = "tilt,timber,time,tiny,tip,tired,tissue,title,toast,tobacco,today,toddler,toe,together,toilet,token";
const char wordlist_chunk_114[] PROGMEM = "tomato,tomorrow,tone,tongue,tonight,tool,tooth,top,topic,topple,torch,tornado,tortoise,toss,total,tourist";
const char wordlist_chunk_115[] PROGMEM = "toward,tower,town,toy,track,trade,traffic,tragic,train,transfer,trap,trash,travel,tray,treat,tree";
const char wordlist_chunk_116[] PROGMEM = "trend,trial,tribe,trick,trigger,trim,trip,trophy,trouble,truck,true,truly,trumpet,trust,truth,try";
const char wordlist_chunk_117[] PROGMEM = "tube,tuition,tumble,tuna,tunnel,turkey,turn,turtle,twelve,twenty,twice,twin,twist,two,type,typical";
const char wordlist_chunk_118[] PROGMEM = "ugly,umbrella,unable,unaware,uncle,uncover,under,undo,unfair,unfold,unhappy,uniform,unique,unit,universe,unknown";
const char wordlist_chunk_119[] PROGMEM = "unlock,until,unusual,unveil,update,upgrade,uphold,upon,upper,upset,urban,urge,usage,use,used,useful";
const char wordlist_chunk_120[] PROGMEM = "useless,usual,utility,vacant,vacuum,vague,valid,valley,valve,van,vanish,vapor,various,vast,vault,vehicle";
const char wordlist_chunk_121[] PROGMEM = "velvet,vendor,venture,venue,verb,verify,version,very,vessel,veteran,viable,vibrant,vicious,victory,video,view";
const char wordlist_chunk_122[] PROGMEM = "village,vintage,violin,virtual,virus,visa,visit,visual,vital,vivid,vocal,voice,void,volcano,volume,vote";
const char wordlist_chunk_123[] PROGMEM = "voyage,wage,wagon,wait,walk,wall,walnut,want,warfare,warm,warrior,wash,wasp,waste,water,wave";
const char wordlist_chunk_124[] PROGMEM = "way,wealth,weapon,wear,weasel,weather,web,wedding,weekend,weird,welcome,west,wet,whale,what,wheat";
const char wordlist_chunk_125[] PROGMEM = "wheel,when,where,whip,whisper,wide,width,wife,wild,will,win,window,wine,wing,wink,winner";
const char wordlist_chunk_126[] PROGMEM = "winter,wire,wisdom,wise,wish,witness,wolf,woman,wonder,wood,wool,word,work,world,worry,worth";
const char wordlist_chunk_127[] PROGMEM = "wrap,wreck,wrestle,wrist,write,wrong,yard,year,yellow,you,young,youth,zebra,zero,zone,zoo";

const char* const wordlist[] PROGMEM = {
    wordlist_chunk_0, wordlist_chunk_1, wordlist_chunk_2, wordlist_chunk_3, wordlist_chunk_4, wordlist_chunk_5, wordlist_chunk_6, wordlist_chunk_7,
    wordlist_chunk_8, wordlist_chunk_9, wordlist_chunk_10, wordlist_chunk_11, wordlist_chunk_12, wordlist_chunk_13, wordlist_chunk_14, wordlist_chunk_15,
    wordlist_chunk_16, wordlist_chunk_17, wordlist_chunk_18, wordlist_chunk_19, wordlist_chunk_20, wordlist_chunk_21, wordlist_chunk_22, wordlist_chunk_23,
    wordlist_chunk_24, wordlist_chunk_25, wordlist_chunk_26, wordlist_chunk_27, wordlist_chunk_28, wordlist_chunk_29, wordlist_chunk_30, wordlist_chunk_31,
    wordlist_chunk_32, wordlist_chunk_33, wordlist_chunk_34, wordlist_chunk_35, wordlist_chunk_36, wordlist_chunk_37, wordlist_chunk_38, wordlist_chunk_39,
    wordlist_chunk_40, wordlist_chunk_41, wordlist_chunk_42, wordlist_chunk_43, wordlist_chunk_44, wordlist_chunk_45, wordlist_chunk_46, wordlist_chunk_47,
    wordlist_chunk_48, wordlist_chunk_49, wordlist_chunk_50, wordlist_chunk_51, wordlist_chunk_52, wordlist_chunk_53, wordlist_chunk_54, wordlist_chunk_55,
    wordlist_chunk_56, wordlist_chunk_57, wordlist_chunk_58, wordlist_chunk_59, wordlist_chunk_60, wordlist_chunk_61, wordlist_chunk_62, wordlist_chunk_63,
    wordlist_chunk_64, wordlist_chunk_65, wordlist_chunk_66, wordlist_chunk_67, wordlist_chunk_68, wordlist_chunk_69, wordlist_chunk_70, wordlist_chunk_71,
    wordlist_chunk_72, wordlist_chunk_73, wordlist_chunk_74, wordlist_chunk_75, wordlist_chunk_76, wordlist_chunk_77, wordlist_chunk_78, wordlist_chunk_79,
    wordlist_chunk_80, wordlist_chunk_81, wordlist_chunk_82, wordlist_chunk_83, wordlist_chunk_84, wordlist_chunk_85, wordlist_chunk_86, wordlist_chunk_87,
    wordlist_chunk_88, wordlist_chunk_89, wordlist_chunk_90, wordlist_chunk_91, wordlist_chunk_92, wordlist_chunk_93, wordlist_chunk_94, wordlist_chunk_95,
    wordlist_chunk_96, wordlist_chunk_97, wordlist_chunk_98, wordlist_chunk_99, wordlist_chunk_100, wordlist_chunk_101, wordlist_chunk_102, wordlist_chunk_103,
    wordlist_chunk_104, wordlist_chunk_105, wordlist_chunk_106, wordlist_chunk_107, wordlist_chunk_108, wordlist_chunk_109, wordlist_chunk_110, wordlist_chunk_111,
    wordlist_chunk_112, wordlist_chunk_113, wordlist_chunk_114, wordlist_chunk_115, wordlist_chunk_116, wordlist_chunk_117, wordlist_chunk_118, wordlist_chunk_119,
    wordlist_chunk_120, wordlist_chunk_121, wordlist_chunk_122, wordlist_chunk_123, wordlist_chunk_124, wordlist_chunk_125, wordlist_chunk_126, wordlist_chunk_127
};

/* Hardware */

LiquidCrystal lcd(LCD_RS, LCD_EN, LCD_D4, LCD_D5, LCD_D6, LCD_D7);
Encoder rotatyEnc(ROTARY_ENCODER_PIN1, ROTARY_ENCODER_PIN2);

/* Hardware state */
static int rotatyEncState = 0;
static int rotatyEncValue = 0;
static bool rotatyEncChanged = false;
static long rotatyEncLastUpdate = 0;


static int buttonOkLastStateMillis = millis();
static bool buttonOkState = false;
static bool buttonOkValue = false;
static bool buttonOkPressed = false;


/* list of words */

#define ENTROPY_LENGTH       256
#define ENTROPY_CHECKSUM     8
#define WORD_BITS            11
#define WORD_COUNT           ((ENTROPY_LENGTH + ENTROPY_CHECKSUM) / WORD_BITS) // 24 words
#define WORD_MAX_VALUE       ((1 << WORD_BITS) - 1)

unsigned short words[WORD_COUNT];


void update_hardware_state()
{
    rotatyEncChanged = false;
    buttonOkPressed = false;

    long now = millis();

    long newPosition = rotatyEnc.read();
    if (newPosition != rotatyEncValue)
        rotatyEncLastUpdate = now;
    if (abs(newPosition - rotatyEncState) >= 4) {
        rotatyEncValue += sign(newPosition - rotatyEncState);
        rotatyEncState = newPosition;
        rotatyEncChanged = true;
    } else if (now - rotatyEncLastUpdate > 100) {
        rotatyEncState = newPosition;
        rotatyEncLastUpdate = now;
    }
    
    int buttonOkDigitalState = 1 - digitalRead(BUTTON_OK_PIN);
    if (buttonOkDigitalState != buttonOkState) {
        buttonOkLastStateMillis = now;
        buttonOkState = buttonOkDigitalState;
    }
    if (buttonOkLastStateMillis + BUTTON_DEBOUNCE_DELAY < now) {
        if (buttonOkState != buttonOkValue) {
            if (!buttonOkState && buttonOkValue) {
                buttonOkPressed = true;
            }
            buttonOkValue = buttonOkState;
        }
    }
}



/**
 * Copy word `index` from `word_list` to `buffer`.
 * Buffer must be at least `WORDLIST_MAX_WORD_LENGTH` + 1 bytes long.
 */
void get_word(char *word, int index)
{
    char buffer[WORDLIST_CHUNK_MAX_LENGTH];


    strcpy_P(buffer, (char*)pgm_read_word(&(wordlist[index / 16])));

    char *p = buffer;
    for (unsigned char i = 0, count = index % 16; i < count; i++)
    {
        p = strchr(p, ',');
        p++;
    }

    memset(word, '*', WORDLIST_MAX_WORD_LENGTH);
    for (unsigned char i = 0; i < WORDLIST_MAX_WORD_LENGTH; i++)
    {
        if (!*p || *p == ',') {
            word[i] = 0;
            break;
        }
        word[i] = *p;
        p++;
    }

    word[WORDLIST_MAX_WORD_LENGTH] = 0;
}

class DebugScreen {
    void draw() {
        lcd.clear();
        lcd.setCursor(0, 0);
        lcd.print("Rotary: ");
        lcd.print(rotatyEncValue);
    }
public:
    void loop() {
        draw();
        while (true) {
            update_hardware_state();
            if (rotatyEncChanged) {
                draw();
            }
        }
    }
};

class MessageScreen {
    char const *line1;
    char const *line2;
    unsigned char hintOk = false;

    void draw() {
        lcd.clear();
        if (hintOk) {
            lcd.setCursor(0, 0);
            //        |XXXXXXXXXXXXXXXX|
            lcd.print("      <OK>      ");
            lcd.setCursor(0, 1);
            lcd.print("   to continue  ");
        } else {
            lcd.setCursor(0, 0);
            lcd.print(line1);
            lcd.setCursor(0, 1);
            lcd.print(line2);
        }
    }
public:
    MessageScreen(char const *line1, char const *line2) {
        this->line1 = line1;
        this->line2 = line2;
    }
    void loop() {
        draw();
        while (true) {
            update_hardware_state();
            if (rotatyEncChanged) {
                hintOk ^= true;
                draw();
            }
            if (buttonOkPressed) {
                break;
            }
        }
    }
};

#define WELCOME_SCREEN_SHOW_NEXT_LINE_DELAY  5000
#define WELCOME_SCREEN_PAGES 3
class WelcomeScreen {
    long screenShowMillis = millis();
    unsigned char pageId = 0;

    void draw() {
        if (pageId == 0) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print(" SEED generator ");
            lcd.setCursor(0, 1);
            lcd.print("(c) BorisBrodski");
        } else if (pageId == 1) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print("    Press OK    ");
            lcd.setCursor(0, 1);
            lcd.print("    to start    ");
        } else if (pageId == 2) {
            lcd.clear();
            lcd.setCursor(0, 0);
            lcd.print(" Rotate pin to  ");
            lcd.setCursor(0, 1);
            lcd.print("enter dice rolls");
        }
    }

public:
    void loop() {
        rotatyEncValue = pageId;
        draw();
        while (true)
        {
            long now = millis();

            update_hardware_state();
            if (rotatyEncChanged) {
                rotatyEncValue = min(WELCOME_SCREEN_PAGES - 1, rotatyEncValue);
                rotatyEncValue = max(0, rotatyEncValue);
                pageId = rotatyEncValue;
                screenShowMillis = now;
                draw();
            }
            if (buttonOkPressed) {
                break;
            }

            if (now - screenShowMillis > WELCOME_SCREEN_SHOW_NEXT_LINE_DELAY) {
                screenShowMillis = now;
                pageId = (pageId + 1) % WELCOME_SCREEN_PAGES;
                rotatyEncValue = pageId;
                draw();
            }
        }
    }
};

unsigned char calculate_sha256() {
    sha256_hasher_t hasher = sha256_hasher_new();

    sha256_hasher_init(hasher);

    /* Serial.println("Words:"); */
    /* for (int i = 0; i < WORD_COUNT; i++) { */
    /*     unsigned short word = words[i]; */
    /*     Serial.print(word); */
    /*     Serial.print(","); */
    /* } */
    /* Serial.println(""); */

    unsigned short bit_idx = 0;
    unsigned char byte_to_hash = 0;
    for (int i = 0; i < WORD_COUNT; i++) {
        unsigned short word = words[i];
        for (signed char j = WORD_BITS - 1; j >= 0; j--) {
            unsigned char bit = (word >> j) & 1;
            byte_to_hash <<= 1;
            byte_to_hash |= bit;
            if (bit_idx % 8 == 7) {
                sha256_hasher_write(hasher, &byte_to_hash, 1);
                byte_to_hash = 0;
            }
            bit_idx++;
            if (bit_idx >= ENTROPY_LENGTH) {
                break; // Skip 8 bits of the last word
            }
        }
    }

    uint8_t * result;
    result = sha256_hasher_gethash(hasher);
    uint8_t checksum = result[0];
    /* Serial.print("\n\nGOT   : "); */
    /* for (int i = 0; i < 32; i++) { */
    /*         Serial.print("0123456789abcdef"[result[i] >> 4]); */
    /*         Serial.print("0123456789abcdef"[result[i] & 0xf]); */
    /* } */
    /* Serial.print("\n"); */
    sha256_hasher_del(hasher);
    return checksum;
}

void correct_last_word(unsigned char checksum) {
    unsigned short * word = &words[WORD_COUNT - 1];
    *word = (*word & 0xff00) | checksum;
}

#define DICE_ROLLS_PER_WORD  5
class EnterEntropyScreen {
    unsigned char words_entered = 0;
    unsigned char dice_rolls_base0[DICE_ROLLS_PER_WORD]; // 0 - 5
    unsigned char dice_rolls_entered = 0;
    unsigned char dice_entering = 3;

    void draw() {
        char line1[17];
        char line2[17];

        //            |W: 12/23, D: 2/5|
        //            |Enter dice 2: 3 |
        sprintf(line1, "W: %2d/%02d, D: %d/%d", words_entered + 1, WORD_COUNT, dice_rolls_entered + 1, DICE_ROLLS_PER_WORD);
        sprintf(line2, "Enter dice %d: %d ", dice_rolls_entered + 1, dice_entering);
        lcd.setCursor(0, 0);
        lcd.print(line1);
        lcd.setCursor(0, 1);
        lcd.print(line2);
    }

    bool calculateAndRecordWord() {
        unsigned short base = 1;
        unsigned short result = 0;
        for (unsigned char i = 0; i < DICE_ROLLS_PER_WORD - 1; i++) {
            result += base * dice_rolls_base0[i];
            base *= 6;
        }
        if (dice_rolls_base0[DICE_ROLLS_PER_WORD - 1] & 1) {
            result += base;
        }
        if (result > WORD_MAX_VALUE) {
            return false;
        }
        words[words_entered] = result;
        words_entered++;
        return true;
    }

    /* void testCalculateAndRecordWord() { */
    /*     int errors = 0; */
    /*     for (unsigned char i1 = 0; i1 < 6; i1++) { */
    /*         for (unsigned char i2 = 0; i2 < 6; i2++) { */
    /*             for (unsigned char i3 = 0; i3 < 6; i3++) { */
    /*                 for (unsigned char i4 = 0; i4 < 6; i4++) { */
    /*                     for (unsigned char i5 = 0; i5 < 6; i5++) { */
    /*                         dice_rolls_base0[0] = i1; */
    /*                         dice_rolls_base0[1] = i2; */
    /*                         dice_rolls_base0[2] = i3; */
    /*                         dice_rolls_base0[3] = i4; */
    /*                         dice_rolls_base0[4] = i5; */
    /*                         words_entered = 0; */
    /*                         if (calculateAndRecordWord()) { */
    /*                             Serial.println(words[0]); */
    /*                         } else { */
    /*                             errors++; */
    /*                         } */
    /*                     } */
    /*                 } */
    /*             } */
    /*         } */
    /*     } */
    /*     Serial.print("Errors: "); */
    /*     Serial.println(errors); */
    /*     Serial.print("All words used ones:"); */
    /* } */

    void showWord(unsigned char wordNumber) {
        char word[WORDLIST_MAX_WORD_LENGTH + 1];
        get_word(word, words[wordNumber - 1]);

        char line1[17];
        char line2[17];
        char spaces[17];
        memset(spaces, ' ', sizeof(spaces));

        //            |    Word #12    |
        //            |     capital    |
        sprintf(line1, "    Word #%d", wordNumber);

        spaces[(16 - strlen(word)) / 2] = 0;
        sprintf(line2, "%s%s", spaces, word);

        MessageScreen(line1, line2).loop();
    }
public:
    void loop() {
        rotatyEncValue = dice_entering;
        draw();

        while (true) {
            update_hardware_state();
                /* dice_entering = rotatyEncValue; */
            if (rotatyEncChanged) {
                rotatyEncValue = max(1, rotatyEncValue);
                rotatyEncValue = min(6, rotatyEncValue);
                dice_entering = rotatyEncValue;
                draw();
            }

            if (buttonOkPressed) {
                dice_rolls_base0[dice_rolls_entered] = dice_entering - 1;
                dice_rolls_entered++;

                if (dice_rolls_entered >= DICE_ROLLS_PER_WORD) {
                    // Calculate word
                    if (calculateAndRecordWord()) {
                        if (words_entered >= WORD_COUNT) {
                            unsigned char checksum = calculate_sha256();
                            correct_last_word(checksum);
                            break;
                        } else {
                            // Next word
                            dice_rolls_entered = 0;
                            dice_entering = 3;
                            draw();
                        }
                    } else {
                        MessageScreen("Can't make word!", " Redo last word ").loop();
                    }

                    dice_rolls_entered = 0;
                }
                dice_entering = 3;
                rotatyEncValue = dice_entering;
                draw();
            }
        }
    }
};

class ShowWordListScreen {
    unsigned char wordIndex = 0;

    void draw() {
        char word[WORDLIST_MAX_WORD_LENGTH + 1];
        get_word(word, words[wordIndex]);
        
        char line1[17];
        char line2[17];

        //            |    Word #12    |
        //            |     capital    |
        sprintf(line1, "    Word #%d  ", wordIndex + 1);

        memset(line2, ' ', sizeof(line2));
        memcpy(line2 + (16 - strlen(word)) / 2, word, strlen(word));
        line2[16] = 0;

        lcd.setCursor(0, 0);
        lcd.print(line1);
        lcd.setCursor(0, 1);
        lcd.print(line2);
    }

public:
    void loop() {
        rotatyEncValue = wordIndex;
        lcd.clear();
        draw();
        while (true) {
            update_hardware_state();
            if (rotatyEncChanged) {
                rotatyEncValue = max(0, rotatyEncValue);
                rotatyEncValue = min(WORD_COUNT - 1, rotatyEncValue);

                if (wordIndex != rotatyEncValue) {
                    wordIndex = rotatyEncValue;
                    draw();
                }
            }

            if (buttonOkPressed) {
                MessageScreen(" Press RESET to ", "  for restart   " ).loop();
                lcd.clear();
                draw();
            }
        }
    }
};

void setup() {
    /* Even not initializing the serial port to prevent information leakage */
    /* Serial.begin(115200); */
    lcd.begin(16, 2);
    lcd.clear();

    pinMode(BUTTON_OK_PIN, INPUT_PULLUP);
}

void loop() {
    /* Serial.print("\nStarting ...\n\n"); */

    /* DebugScreen().loop(); */
    WelcomeScreen().loop();
    EnterEntropyScreen().loop();
    ShowWordListScreen().loop();

}

