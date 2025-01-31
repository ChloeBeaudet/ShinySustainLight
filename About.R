function(){
  tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:bold;">About</span></a>'),
           HTML('
      <div style="display:inline;float:right;margin:0px 0px 30px 20px">
        <img src="image_about.png" border="5" width="500" style="margin:0px"/>
      
      </div>

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify">
         
         This web application is associated with the project 
         <a href = "https://www.latelescop.fr/recherche-developpement/#:~:text=Le%20projet%20POLLUM%2C%20men%C3%A9%20sur,%C3%A0%20tr%C3%A8s%20haute%20r%C3%A9solution%20spatiale." target =_blank>\"<strong>POLLUM: Cr\u00e9ation 
         d\'un outil d\'aide \u00e0 la d\u00e9finition de la trame noire \u00e0 partir d\'images 
         satellites nocturnes \u00e0 tr\u00e8s haute r\u00e9solution</strong>\"</a> (<i>POLLUM: Tool for the definition 
         of a dark infrastructure from very high-resolution night-time satellite images</i>) 
         funded by the Occitanie region and led by La Telescop and INRAE Montpellier, and with the article "Planning sustainable lighting for biodiversity and society" ([1]). 
         The objective is to develop and evaluate a methodology for identifying the 
         \"<strong>Trame noire</strong>\" (<i>Dark infrastructure</i>). To this end, three analyses were carried out: 
         analysis of (i) light pollution and (ii) ecological stakes, led by La Telescop supported by 
         three naturalist associations (OPIE, LPO and Groupe de Chiropt\u00e8re du Languedoc Roussillon),
         and (iii) an analysis of social acceptability of mitigation measures coordinated by INRAE 
         Montpellier (UMR TETIS). The report ([2]) (in french) is available <a href = "https://hal.science/hal-04573796">here</a>.
        </p>
      </div>   

      <br>
      
      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify">
          The web app is complementary to the project report, enabling easier,
          more flexible and user-friendly visualization of the results. It is composed of four tabs that we present here in reverse order: 
        </p>
      </div> 

     <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify;">
       <ul> <li style="font-size:120%;text-align:justify"> The last tab, "<strong>Light Pollution Indicator</strong>", 
                presents the indicator from sections II.B.3 of [2] and 2.1 of [1], which measures upward emissions on 
                a 1-meter grid. This data was estimated using high-resolution RGB images from the Jilin-1 satellite 
                over the Montpellier Metropolitan Area during cloud-free nights in 2022. Indicators for visible light
                sources within 500m and 100m, relevant for assessing light pollution\'s impact on insect and bat dispersion,
                as detailed in II.B.3 of [2] and 2.1 of [1], are too data-intensive and take too long to load in this app 
                but can be provided upon request.
     
     
     </li>
           </p>
      </div>

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify;">
       <ul> <li style="font-size:120%;text-align:justify"> In the third tab, "<strong>Ecological Indicators</strong>", you can visualize maps of 
          ecological stakes related to light pollution in the Montpellier Metropolitan Area (MMA) (section 2.2 of [1]). 
          Six groups of species particularly sensitive to light pollution are represented: european nightjars, amphibians,
          insects living in wetlands, lampyridae and two bats genera: Myotis spp and Rhinolophus spp. 
          For each groups of species, you can view (i) the predicted impact of light pollution on dispersion according to ecological stakes 
        (evaluated in the absence of light pollution) (Section V.A.1 of [2]), and (ii) on biodiversity reservoirs (Section III.B of [2]). 
     The application does not allow to visualize the impact of light pollution on amphibian dispersion, due to the small number of areas affected. 
     Biodiversity reservoirs for bats (Rhinolophus spp and Myotis spp) cannot be visualized either, due to data confidentiality. 
     Additionally, this tab provides global indicators of ecological stakes. 
          The "overall score" is derived from the combination of ecological stakes without light pollution (high or moderate),
          functionality loss of potential in reservoirs and dispersion areas (high, moderate, or low), and the number of affected species groups 
          (from 1 to 2 or from 3 to 6). "Priority areas for light pollution mitigation policies" corresponds 
          to the same indicator allocated to the relevant light-emitting point (Section V.C.1 of [2]).</li>
           </p>
      </div>  
      
      

      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify;">
        <ul> <li style="font-size:120%;text-align:justify"> The second tab, \"<strong>Socio-economic indicators</strong>\", corresponds to the social 
        acceptability indicators of public lighting extinction (Section IV.D of [2]) 
        (based on two publications, [3] and [4]). 
        The indicators correspond to an average \"acceptability score\" per IRIS 
        (i.e., an infra-municipal level) for two policies: light extinction 
        from 1 a.m. to 5 a.m. and from 11 p.m. to 6 a.m. (Table 1)</li>
           </p>
      </div>  
      
      <div style="max-width:1000px; word-wrap:break-word;">
        <p style="font-size:120%;text-align:justify;">
        <ul> <li style="font-size:120%;text-align:justify">The first tab \"<strong>Summary map</strong>\", displays a summary map of the ecological stakes and social acceptability of light pollution mitigation measures.
        It displays bivariate maps of the social acceptability score (either for 
        extinction from 1 a.m. to 5 a.m. or for extinction from 11 p.m. to 6 a.m.) 
        and the previously described overall ecological indicators \"Priority areas for light pollution mitigation policies\", either for
        high or moderate ecological stakes without light pollution (Figure 1).</li>
           </p>
      </div>  

      <hr width="1000", align="left" style="height:0.5px;border:none;color:#A0A5A8;background-color:#A0A5A8;" />

		  <span style="color:#64645F;font-weight: bold;">Contributors</span>
  <div style="max-width:1000px; word-wrap:break-word;">

    <p style="text-align:justify">
      Sarah Potin & Julie Chaurand (<a href="https://www.latelescop.fr/" target=_blank>La Telescop</a>)
    </p>

      <p style="text-align:justify">
        <a href="https://leatardieu.wordpress.com/" target=_blank>L\u00e9a Tardieu</a> (INRAE, TETIS and CIRED), <a href="https://chloebeaudet.github.io/" target=_blank>Chlo\u00e9 Beaudet</a>, <a href="https://eng-psae.versailles-grignon.hub.inrae.fr/personalpages/david" target=_blank>Maia David</a> (AgroParisTech, PSAE) &
          <a href="https://cesco.mnhn.fr/fr/annuaire/lea-mariton-6464" target=_blank>L\u00e9a Mariton</a> (CESCO, MNHN)
          </p>
            </div>


      <span style="color:#64645F;font-weight:bold;">References</span>
		  <div style="max-width:1000px; word-wrap:break-word;">
		     <p style="text-align:justify;">
     
         [1] Tardieu L, Beaudet C, Potin S, Chaurand J, Mariton L, David M (submitted). Planning sustainable lighting for biodiversity and society.
         </p>
     
        <p style="text-align:justify;">
         [2] Potin S, Chaurand J, Beaudet C, Tardieu L. <a href = "https://hal.science/hal-04573796"> POLLUM : Cr\u00e9ation d\'un outil d\'aide \u00e0 la d\u00e9finition de la trame noire \u00e0 partir d\'images satellites nocturnes \u00e0 tr\u00e8s haute r\u00e9solution : Rapport Final.</a> La TeleScop; INRAE. 2024, 66 p.
     </p>
		     <p style="text-align:justify;"> 
		     [3] Beaudet C, Tardieu L, David M (2022). <a href="https://www.sciencedirect.com/science/article/pii/S0921800922001896" target=_blank>Are citizens willing to accept changes in public lighting for biodiversity conservation?</a> <i>Ecological Economics</i> 200, 107527, <a href="https://www.sciencedirect.com/science/article/pii/S0921800922001896" target=_blank>https://doi.org/10.1016/j.ecolecon.2022.107527</a>
		     </p>
		  
		  <p style="text-align:justify;">
		     
		     [4] Beaudet C, Tardieu L, Crastes Dit Sourd R, David M (submitted). <a href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4931486" target=_blank>Mapping preferences derived from a choice experiment: a comparison of two methods.</a>
		     
		     </p>
		  </div> 
		  

		  
		 
		'),
           value = "about"
  )
}


